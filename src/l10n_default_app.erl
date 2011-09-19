-module(l10n_default_app, [App]).
-extends(l10n_default).
-define(DEF, l10n_default).

-export([get_name/1, get_path/2, source_files/0]).

get_name(X) -> ?DEF:get_name(X).

get_path(Type, Locale) ->
	code:priv_dir(App) ++ "/translates/" ++ atom_to_list(Locale) ++ "." ++ atom_to_list(Locale).

source_files() ->
    DirName = code:lib_dir(App, src),
    Files = filelib:wildcard(DirName ++ "/*.erl").
