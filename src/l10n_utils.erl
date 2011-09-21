-module(l10n_utils).
-export([hash/1, format/1, string/1, file_write/2]).

hash(X) -> erlang:phash2(X).
format(X) -> i18n_message:open(X).
string(X) -> i18n_string:from(X).

file_write(FileName, IOList) -> 
    {ok, FD} = file:open(FileName, [write]), 
    io:format(FD, "~ts", [IOList]), 
    file:close(FD),
	ok.
