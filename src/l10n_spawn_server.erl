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
-module(l10n_spawn_server).

-export([start_link/0]).
-export([init/1, terminate/2, 
    handle_call/3, handle_cast/2]).
%% Exported Client Functions
-export([find_table/1, find_store/1, reg/3]).

-behavior(gen_server).

-define(CFG_TBLNAME_STORES, 'l10n_stores').
-define(CFG_TBLNAME_TABLES, 'l10n_tables').


%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    ets:new(?CFG_TBLNAME_STORES, [{read_concurrency, true}, named_table]),
    ets:new(?CFG_TBLNAME_TABLES, [{read_concurrency, true}, named_table]),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.


handle_call({spawn_store, Key}, _From, LoopData) ->
    Reply = Pid = spawn_store(Key),
%   waiter:subscribe(Pid),

    ets:insert(?CFG_TBLNAME_TABLES, {Key, Pid}),
    {reply, Reply, LoopData}.


handle_cast({reg, Key, Table, Pid}, LoopData) ->
    erlang:link(Pid),

    ets:insert(?CFG_TBLNAME_STORES, {Key, Pid}),
    ets:insert(?CFG_TBLNAME_TABLES, {Key, Table}),
    {noreply, LoopData}.

    
%%
%% API
%%

find_store(Locale) ->
    case lookup(?CFG_TBLNAME_STORES, Locale) of
    'undefined' ->
        WaiterPid = try_spawn(Locale),
        % Wait full initialization
        {StorePid, _TableId} = waiter:wait(WaiterPid),
        StorePid;
    Pid -> Pid
    end.

find_table(Locale) ->
    case lookup(?CFG_TBLNAME_TABLES, Locale) of
    Table when is_integer(Table) -> Table;
    WaiterPid when is_pid(WaiterPid) -> 
        {_StorePid, TableId} = waiter:wait(WaiterPid),
        TableId;
    'undefined' ->
        Pid = find_store(Locale),
        l10n_store_server:get_table(Pid)
    end.
        

%%
%% Server API
%%
        
try_spawn(Locale) ->
    call({spawn_store, Locale}).

reg(Key, Table, Pid) ->
    gen_server:cast(?MODULE, {reg, Key, Table, Pid}).


%%
%% Helpers
%%


spawn_store(Locale) ->
    {ok, Pid} = l10n_store_server:start_link(Locale),
    waiter:spawn(fun() ->
        TableId = l10n_store_server:get_table(Pid),
        {Pid, TableId}
        end).

call(Cmd) ->
    try
        gen_server:call(?MODULE, Cmd)
    catch
        exit:{noproc, _Stack} ->
        l10n:start(),
        gen_server:call(?MODULE, Cmd)
    end.

lookup(Table, Key) ->
    % Key is a locale
    try
    [{Key, Value}] = ets:lookup(Table, Key),
    Value
    catch
        error:badarg -> 
        % Server is not running.
        l10n:start(),
        lookup(Table, Key);

        error:{badmatch, _V} ->
        % Key is not found.
        'undefined'
    end.

