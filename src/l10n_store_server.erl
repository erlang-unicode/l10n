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

-export([start_link/0]).
-export([init/1, terminate/2, 
    handle_call/3, handle_cast/2]).
-export([get_table/1]).


-export([search_format/2, load_format/2]).
-export([search_string/2, load_string/2]).

-behavior(gen_server).

-record(state, {table}).
-define(CFG_TABLE_NAME, 'l10n_server_locale_store').

%% Exported Client Functions
%% Operation & Maintenance API
start_link() ->
    Arguments = [],
    Opts = [],
    gen_server:start_link(?MODULE, Arguments, Opts).

init([]) ->
    E = ets:new(?CFG_TABLE_NAME, [{'read_concurrency', true}]),
    {ok, [#state{table=E}]}.

terminate(_Reason, _LoopData) ->
    ok.



handle_call('get_table', _From, LoopData=#state{table=E}) ->
    {'reply', E, LoopData}.

handle_cast({'update_value', Key, Value}, LoopData=#state{table=E}) ->
    ets:insert(E, {Key, Value}),
    {'noreply', LoopData}.

get_table(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, 'get_table').
    
    

search_string(Locale, StrHash) ->
    search(Locale, StrHash).
    

load_string(Locale, Str) ->
	Hash = erlang:phash2(Str), 
    Res = i18n_string:from(Str),
    Pid = l10n_spawn_server:find_store(Locale),
    gen_server:call(Pid, {'update_value', Hash, Res}),
    Res.
    
	

search_format(Locale, StrHash) ->
    search(Locale, StrHash).

load_format(Locale, Str) ->
	Hash = erlang:phash2(Str), 
    UStr = i18n_string:from(Str),
    Res = i18n_message:open(Locale, UStr),
    Pid = l10n_spawn_server:find_store(Locale),
    gen_server:call(Pid, {'update_value', Hash, Res}),
    Res.
	

search(Locale, StrHash) ->
    Table = l10n_spawn_server:find_table(Locale),
    ets:lookup(Table, StrHash).
