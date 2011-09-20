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

-export([start_link/2]).

%% gen_server handlers
-export([init/1, terminate/2, 
    handle_call/3, handle_cast/2]).

%% API
-export([get_table/1, update_value/3]).


-behavior(gen_server).
-record(state, {domain, table, locale}).

%% Exported Client Functions
%% Operation & Maintenance API
start_link(D, L) ->
    Arguments = [D, L],
    Opts = [],
    gen_server:start_link(?MODULE, Arguments, Opts).

init([D, L]) ->
    E = ets:new('l10n_store_table', [{'read_concurrency', true}]),
    init_store(self()),
    {ok, #state{table=E, locale=L, domain=D}}.

terminate(_Reason, _LoopData) ->
    ok.



handle_call('get_table', _From, LoopData=#state{table=E}) ->
    {'reply', E, LoopData}.


handle_cast('init_store', LoopData=#state{table=E}) ->
    {'noreply', LoopData};
handle_cast({'update_value', Key, Value}, LoopData=#state{table=E}) ->
    ets:insert(E, {Key, Value}),
    {'noreply', LoopData}.


%%
%% User API
%%

get_table(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, 'get_table').

init_store(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, 'init_store').
    
update_value(Pid, Key, Value) when is_pid(Pid) ->
    gen_server:cast(Pid, {'update_value', Key, Value}).


