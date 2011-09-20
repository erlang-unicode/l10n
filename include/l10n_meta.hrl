
-export([get_path/2, get_type/0, get_name/1, source_files/0]).
-export([start_link/0]).
-export([format/2, string/1]).

-ifdef(L10N_APPLICATION).
-define(L10N_PATH(T, L),
	begin
		code:priv_dir(?L10N_APPLICATION) ++ "/translates/" 
			++ atom_to_list(L) ++ "." ++ atom_to_list(T)
	end).

-define(L10N_SOURCE,
	begin
    	DirName = code:lib_dir(?L10N_APPLICATION, src),
    	filelib:wildcard(DirName ++ "/*.erl")
	end).
-endif.

-ifndef(L10N_SERVER).
-define(L10N_SERVER, ?MODULE).
-endif.

-ifndef(L10N_TABLE).
-define(L10N_TABLE, ?MODULE).
-endif.

-ifndef(L10N_TYPE).
-define(L10N_TYPE, 'string').
-endif.


-type l10n_file_type() :: 'po' | 'pot'.
-type l10n_locale() :: atom().

-spec get_path(l10n_file_type(), l10n_locale()) -> string().
get_path(Type, Locale) ->
	?L10N_PATH(Type, Locale).

get_name('domain') -> ?MODULE;
get_name('table')  -> ?L10N_TABLE;
get_name('server') -> ?L10N_SERVER.

source_files() ->
	?L10N_SOURCE.

%% format | string
get_type() -> ?L10N_TYPE.

start_link() ->
	l10n_spawn_server:start_link(?MODULE).

format(Id, Params) ->
	?L10N_TYPE = 'format',
	H = l10n_utils:hash(Id),
	format(H, Id, Params).

format(H, Id, Params) ->
	Fmt = case extract(H, 5) of
		false -> 
			X = l10n_utils:format(Id), 
			insert(H, X),
			X;
		X -> X
		end,
	i18n_message:format(Fmt, Params).
	

string(Id) ->
	?L10N_TYPE = 'string',
	H = l10n_utils:hash(Id),
	string(H, Id).

string(H, Id) ->
	?L10N_TYPE = 'string',
	H = l10n_utils:hash(Id),
	case extract(H, 5) of
	[] ->
		X = l10n_utils:string(Id), 
		insert(H, X),
		X;
	[{_,X}] -> X
	end.

set_locale(L) ->
	set_table(L).

extract(H, Count) 
	when Count > 0 ->
	V = try
		T = get(?L10N_TABLE),
		ets:lookup(T, H)
	catch error:_ ->
		L = l10n_locale:get_locale(),
		set_table(L),
		extract(H, Count - 1)
	end.

set_table(L) ->
	P = l10n_spawn_server:find_store(?MODULE, L),
	T = l10n_store_server:get_table(P),
	put(?L10N_TABLE, T),
	% When user calls l10n_locale:set_locale(...),
	% this tid will be erased.
	l10n_locale:add_domain(?MODULE),
	T.

insert(Key, Value) ->
	L = l10n_locale:get_locale(),
	P = l10n_spawn_server:find_store(?MODULE, L),
	l10n_store_server:update_value(P, Key, Value).
	
