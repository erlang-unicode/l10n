% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:

%%% =====================================================================
%%%   Copyright 2011 Uvarov Michael 
%%%
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%
%%% $Id$
%%%
%%% @copyright 2010-2011 Michael Uvarov
%%% @author Michael Uvarov <freeakk@gmail.com>
%%% =====================================================================

%%% @private
-module(l10n_store_server).

-export([start_link/2, stop/1]).

%% gen_server handlers
-export([init/1, terminate/2, 
    handle_call/3, handle_cast/2, handle_info/2]).

%% API
-export([get_table/1, get_locale/1, update_value/3]).


-behavior(gen_server).
-record(state, {domain, table, locale, ref}).

%% Exported Client Functions
%% Operation & Maintenance API
start_link(D, L) ->
    Arguments = [D, L],
    Opts = [],
    gen_server:start_link({local, D:get_store_server_name(L)}, ?MODULE, Arguments, Opts).

%% @spec stop() -> ok
%% @doc Stop the server.
stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, 'stop').


init([D, L='root']) ->
    E = ets:new('l10n_root_store_table', [{'read_concurrency', true}]),
    {ok, #state{table=E, locale=L, domain=D}};

init([D, L]) ->
    E = ets:new('l10n_store_table', [{'read_concurrency', true}]),
    init_store(self()),
    {ok, #state{table=E, locale=L, domain=D}}.

terminate(_Reason, _LoopData) ->
    ok.



handle_call('get_table', _From, LoopData=#state{table=E}) ->
    {'reply', E, LoopData};
handle_call('get_locale', _From, LoopData=#state{locale=L}) ->
    {'reply', L, LoopData};

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call('stop', _From, LoopData) ->
    {stop, shutdown, stopped, LoopData}.


handle_cast('init_store', LoopData=#state{table=E, domain=D, locale=L}) ->
    % Get the copy of the parent table
    Ref = case l10n_locale:get_parent_locale(L) of
    'root' -> 
        % If the root server was terminated, kill this server
	    P = l10n_spawn_server:find_store(D, 'root'),
        erlang:monitor('process', P);
    X -> 
        % If the parent server was terminated, kill this server
	    P = l10n_spawn_server:find_store(D, X),
        From = get_table(P),
        l10n_utils:copy_table(From, E),
        erlang:monitor('process', P)
    end,


    PO = D:get_file('po', L),
    F = fun(Key, Value, EE) -> 
            ets:insert(EE, {l10n_utils:hash(Key), D:string_to_object(Value)})
        end,
    l10n_export:fold(F, E, PO),
    {'noreply', LoopData#state{ref=Ref}};
handle_cast({'update_value', Key, Value}, LoopData=#state{table=E}) ->
    ets:insert(E, {Key, Value}),
    {'noreply', LoopData}.

handle_info({'DOWN', Ref, 'process', _PPid,  _Reason}, LoopData=#state{ref=Ref}) -> 
    {'stop', 'normal', LoopData}.


%%
%% User API
%%

get_table(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, 'get_table').

get_locale(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, 'get_locale').

init_store(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, 'init_store').
    
update_value(Pid, Key, Value) when is_pid(Pid) ->
    gen_server:cast(Pid, {'update_value', Key, Value}).


