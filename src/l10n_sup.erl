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

-module(l10n_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(D) ->
    supervisor:start_link({local, D:get_name('supervisor')}, ?MODULE, [D]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([D]) ->
    SpawnServer = {l10n_spawn_server,
        {l10n_spawn_server, start_link, [D]},
        permanent, 10000, worker, [l10n_spawn_server]},

    Reloader = {l10n_reloader,
        {l10n_reloader, start_link, [D]},
        permanent, 10000, worker, [l10n_reloader]},

    {ok, { {one_for_one, 5, 10}, [SpawnServer, Reloader]} }.

