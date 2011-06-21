%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Email MIME encoding library.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_mime).

-include_lib("esmtp_mime.hrl").

%% API
-export([encode/1, send/5,
         msg/0, msg/3, msg/4, msg/5,
         from/1, to/1,
         add_text_part/2]).

-export([test_msg/0,
         send_test/4,
         test/0]).

%%====================================================================
%% API
%%====================================================================

msg() ->
    #mime_msg{boundary=invent_mime_boundary(),
              headers=[{"Date", httpd_util:rfc1123_date()}]}.

msg(To, From, Subject) ->
    msg(To, From, Subject, undefined).

msg(To, From, Subject, TextBody) ->
    msg(?DEFAULT_MESSAGE_TYPE, To, From, Subject, TextBody).

msg(Type, To, From, Subject, TextBody) ->
    Msg = #mime_msg{boundary=invent_mime_boundary(),
              type=Type,
              headers=[{"To", To},
                       {"Subject", Subject},
                       {"From", From},
                       {"Date", httpd_util:rfc1123_date()}
                      ]},
    
    add_text_part(Msg, TextBody).

encode(Msg) ->
    encode_headers(headers(Msg)) ++ "\r\n\r\n" ++
        encode_parts(Msg) ++
        "--" ++ Msg#mime_msg.boundary ++ "--\r\n".

to(#mime_msg{headers=H}) ->
    proplists:get_value("To", H, undefined).

from(#mime_msg{headers=H}) ->
    proplists:get_value("From", H, undefined).
    
add_text_part(Msg, undefined) ->
    Msg;
add_text_part(Msg = #mime_msg{parts=Parts}, Text) ->
    Msg#mime_msg{parts=Parts ++ [#mime_part{data=Text}]}.
    
send(Ip, Host, From, To, Msg=#mime_msg{}) ->
    ok = smtpc:sendmail(Ip, Host, From, To, encode(Msg)).

%%====================================================================
%% Internal functions
%%====================================================================

test_msg() ->
    #mime_msg{boundary=invent_mime_boundary(),
              headers=[{"To", "Geoff Cant <geoff@example.com>"},
                       {"Subject", "Daily Report"},
                       {"From", "Geoff Cant <geoff@example.com>"},
                       {"Date", httpd_util:rfc1123_date()}
                      ],
              parts=[#mime_part{data="This is a test..."},
                     #mime_part{data="This,is,a,test\r\nof,something,ok,maybe",
                                type=attachment,
                                encoding={"7bit","text/plain","iso-8859-1"},
                                name="foo.csv"}]}.
test() ->
    io:format("~s~n", [encode(test_msg())]).

send_test(Ip, Host, From, To) ->
    send(Ip, Host, From, To, test_msg()).

encode_header({Header, [V|Vs]}) when is_list(V) ->
    Hdr = lists:map(fun ({K, Value}) when is_list(K), is_list(Value) ->
                            K ++ "=" ++ Value;
                        ({K, Value}) when is_atom(K), is_list(Value) ->
                            atom_to_list(K) ++ "=" ++ Value;
                        (Value) when is_list(Value) -> Value
                    end,
                    [V|Vs]),
    Header ++ ": " ++ join(Hdr, ";\r\n  ");
encode_header({Header, Value}) when is_list(Header), is_list(Value) ->
    Header ++ ": " ++ Value;
encode_header({Header, Value}) when is_atom(Header), is_list(Value) ->
    atom_to_list(Header) ++ ": " ++ Value.

encode_headers(PropList) ->
    join(lists:map(fun encode_header/1,
                   PropList),
         "\r\n").

encode_parts(#mime_msg{parts=Parts, boundary=Boundary}) ->
    lists:map(fun (P) -> encode_part(P,Boundary) end, Parts).

encode_part(#mime_part{data=Data} = P, Boundary) ->
    "--" ++ Boundary ++ "\r\n" ++
    encode_headers(part_headers(P)) ++ "\r\n\r\n" ++
    Data ++ "\r\n".

part_headers(#mime_part{type=undefined, encoding={Enc, MimeType, Charset},
                        name=undefined}) ->
    [{"Content-Transfer-Encoding", Enc},
     {"Content-Type", [MimeType, {charset, Charset}]}];
part_headers(#mime_part{type=Type, encoding={Enc, MimeType, Charset},
                        name=Name}) when Type==inline; Type == attachment ->
    [{"Content-Transfer-Encoding", Enc},
     {"Content-Type", [MimeType, "charset=" ++ Charset ++ ",name=" ++ Name]},
     {"Content-Disposition", [atom_to_list(Type), 
                              {"filename", 
                              Name}]}].

headers(#mime_msg{type=Type, headers=H, boundary=Boundary}) ->
    H ++ [{"MIME-Version", "1.0"},
          {"Content-Type", [Type, 
                            "boundary=\"" ++ Boundary ++ "\""]}].

invent_mime_boundary() ->
    string:copies("=", 10) ++ list_rand(boundary_chars(), 30).
        
list_rand(List, N) ->
    lists:map(fun (_) -> list_rand(List) end,
              lists:seq(1,N)).

list_rand(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List).

boundary_chars() ->
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789"
%    "'()+_,-./=?"
    .

join([H1, H2| T], S) when is_list(H1), is_list(H2), is_list(S) ->
    H1 ++ S ++ join([H2| T], S);
%join([C1, C2 | Chars], S) when is_integer(C1), is_integer(C2), is_list(S) ->
%    [C1|S] ++ S ++ join([C2 | Chars], S);
join([H], _) ->
    H;
join([], _) ->
    [].
    
%%====================================================================
%% Eunit
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

msg_test() ->
    {ok, P1} = re:compile("Date: .*\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed;\r\n  boundary=\"==========.*\"\r\n\r\n--==========.*--\r\n"),
    ?assertMatch({match, _}, re:run(encode(msg()), P1)),

    {ok, P2} = re:compile("To: to\r\nSubject: subject\r\nFrom: from\r\nDate: .*\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed;\r\n  boundary=\"==========.*\"\r\n\r\n--==========.*--\r\n"),
    ?assertMatch({match, _}, re:run(encode(msg("to", "from", "subject")), P2)),
    
    {ok, P3} = re:compile("To: to\r\nSubject: subject\r\nFrom: from\r\nDate: .*\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed;\r\n  boundary=\"==========.*\"\r\n\r\n--==========.*\r\nContent-Transfer-Encoding: 7bit\r\nContent-Type: text/plain;\r\n  charset=iso-8859-1\r\n\r\ntext body\r\n--==========.*--\r\n"),
    ?assertMatch({match, _}, re:run(encode(msg("to", "from", "subject", "text body")), P3)),
    
    {ok, P4} = re:compile("To: to\r\nSubject: subject\r\nFrom: from\r\nDate: .*\r\nMIME-Version: 1.0\r\nContent-Type: multipart/alternative;\r\n  boundary=\"==========.*\"\r\n\r\n--==========.*\r\nContent-Transfer-Encoding: 7bit\r\nContent-Type: text/plain;\r\n  charset=iso-8859-1\r\n\r\ntext body\r\n--==========.*--\r\n"),
    ?assertMatch({match, _}, re:run(encode(msg(?MULTIPART_ALTERNATIVE_TYPE, "to", "from", "subject", "text body")), P4)),
    ok.

-endif.
