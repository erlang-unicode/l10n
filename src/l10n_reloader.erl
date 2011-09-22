% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:

%%% =========================================================================
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
%%% =========================================================================

%%% =========================================================================
%%% This is the MIT license.
%%% 
%%% Copyright (c) 2007 Mochi Media, Inc.
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a 
%%% copy of this software and associated documentation files (the "Software"), 
%%% to deal in the Software without restriction, including without limitation 
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense, 
%%% and/or sell copies of the Software, and to permit persons to whom the 
%%% Software is furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included 
%%% in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
%%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @copyright 2007 Mochi Media, Inc.
%%% @author Matthew Dempsky <matthew@mochimedia.com>
%%% =========================================================================

-module(l10n_reloader).
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {last, tref, domain, available}).

%% External API

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start_link(D) ->
	Params = [D],
	Opts = [],
    gen_server:start_link({local, D:get_name('reloader')}, ?MODULE, Params, Opts).

%% gen_server callbacks

%% @spec init([Domain]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([D]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(15), doit),
    AL = lists:sort(D:available_locales()),
    {ok, #state{last=stamp(), tref=TRef, domain=D, available=AL}}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    AL = doit(State, Now),
    {noreply, State#state{last=Now, available=AL}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.


%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% Internal API

doit(#state{domain=D, available=OldAL, last=From}, To) ->
    AL = lists:sort(D:available_locales()),
    LL = lists:sort(D:loaded_locales() -- ['root']),
    F = fun(Mtime) -> Mtime >= From andalso Mtime < To end,
    
    do_map(D, F, OldAL, AL, LL),
    AL.


do_map(D, F, [L|TOAL], [L|TAL], [L|TLL]) ->
    FileName = D:get_path('po', L),

     {ok, I} = file:read_file_info(FileName),
     case F(I#file_info.mtime) of
     true -> 
         reload(D, L);
     false ->
         'unmodified' 
    end,

    do_map(D, F, TOAL, TAL, TLL);

do_map(D, F, [HOAL|TOAL]=OAL, [HAL|TAL]=AL, [HLL|TLL]=LL) ->
    case lists:min([HOAL, HAL, HLL]) of
    HOAL when HAL =:= HOAL ->
        % HOAL was unloaded.
        do_map(D, F, TOAL, TAL, LL);

    HOAL when HLL =:= HOAL ->
        % HOAL was unloaded and deleted.
        do_map(D, F, TOAL, AL, TLL);

    HOAL ->
        % HOAL was deleted.
        do_map(D, F, TOAL, AL, TLL);

    HAL when HLL =:= HAL ->
        % The new translation file for HAL was added.
        % Load HAL, because HAL is used without loading.
        reload(D, HAL),
        do_map(D, F, OAL, TAL, TLL);

    HAL ->
        % The new translation file for HAL was added.
        % HAL is unused.
        do_map(D, F, OAL, TAL, LL);

    HLL ->
        % HLL was started to be using.
        % HLL has not a translation file.
        do_map(D, F, OAL, AL, TLL)
    end;
        
do_map(D, _F, _OAL, _AL, LL) ->
    do_ll(D, LL).
 
%% @doc PO files were deleted.
do_ll(D, [H|T]) ->
    reload(D, H),
    do_ll(D, T);
do_ll(_D, []) ->
    ok.

reload(D, L) ->
    io:format("Reload ~s~n", [L]),
    D:reload(L).


stamp() ->
    erlang:localtime().

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
