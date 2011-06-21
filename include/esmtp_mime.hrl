%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mail Mime library headers and record definitions.
%% @end

-ifndef(esmtp_mime).
-define(esmtp_mime, true).

-define(MULTIPART_MIXED_TYPE, "multipart/mixed").
-define(MULTIPART_ALTERNATIVE_TYPE, "multipart/alternative").
-define(DEFAULT_MESSAGE_TYPE, ?MULTIPART_MIXED_TYPE).

-record(mime_msg, {type= ?DEFAULT_MESSAGE_TYPE,
                   headers = [],
                   boundary, parts = []}).

-record(mime_part, {type,
                    encoding = {"7bit", "text/plain","iso-8859-1"},
                    name,
                    data}).

-endif.
