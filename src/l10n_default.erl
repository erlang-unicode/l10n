-module(l10n_default).
-export([get_path/2, get_name/1, source_files/0]).

-type l10n_file_type() :: 'po' | 'pot'.
-type l10n_locale() :: atom().

-spec get_path(l10n_file_type(), l10n_locale()) -> string().
get_path(Type, Locale) ->
	erlang:throw(abstract_method).

get_name('domain') -> 'default';
get_name('table_name') -> 'l10n_default';
get_name('server_name') -> 'l10n_default';
get_name('spawn_server') -> 'l10n_default'; % name for a named server
get_name('store_server') -> 'l10n_default'.

source_files() ->
	erlang:throw(abstract_method).

