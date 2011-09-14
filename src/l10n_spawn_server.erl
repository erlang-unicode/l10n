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
    handle_call/3]).
-export([find_store/1]).

-behavior(gen_server).

-define(CFG_TBLNAME_STORES, 'l10n_stores').
-define(CFG_TBLNAME_TABLES, 'l10n_tables').


%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arguments, Opts).

init([]) ->
    E = ets:new(?CFG_TABLE_NAME, [{read_concurrency, true}, named_table]),
    {ok, []}.

terminate(_Reason, _LoopData) ->
    ok.


handle_call({spawn_store, Locale}, _From, LoopData) ->
    {ok, Pid} = l10n_store_server:start_link(Locale),
    
    Reply = ets:insert(?CFG_TABLE_NAME, {Locale, Pid}),
    {reply, Reply, LoopData}.

    
%%
%% API
%%

find_store(Locale) ->
    lookup(?CFG_TBLNAME_STORES, Locale).
find_table(Locale) ->
    lookup(?CFG_TBLNAME_TABLES, Locale).

try_spawn(Locale) ->
    call({spawn_store, Locale}).
        

%%
%% Helpers
%%

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
        lookup(Key);

        error:{badmatch, _V} ->
        % Key is not found.
        try_spawn(Key)
    end.

