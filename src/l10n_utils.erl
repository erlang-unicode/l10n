-module(l10n_utils).
-export([hash/1, format/1, string/1, file_write/2, copy_table/2, get_keys/1]).

hash(X) -> erlang:phash2(X).
format(X) -> i18n_message:open(string(X)).
string(X) -> i18n_string:from(X).

file_write(FileName, IOList) -> 
    {ok, FD} = file:open(FileName, [write]), 
    io:format(FD, "~ts", [IOList]), 
    file:close(FD),
	ok.

copy_table(From, To) ->
	List = ets:tab2list(From),
	true = ets:insert(To, List).

get_keys(Table) ->
	P = ets:info(Table, 'keypos'),
	[element(P, X) || X <- ets:tab2list(Table), is_tuple(X), tuple_size(X) >= P].
