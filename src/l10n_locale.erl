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
%%% @author Michael Uvarov <arcusfelis@gmail.com>
%%% =====================================================================

-module(l10n_locale).
-export([set_locale/1, get_locale/0]).
-export([get_parent_locale/1]).
-export([add_domain/1]).


%%
%% Client API
%%

set_locale(Value) -> 
    LName = i18n_locale:base_name(Value),
    put('l10n_locale', LName),
    delete_tids(),
    LName.

get_parent_locale(Locale) ->
    i18n_locale:parent_locale(Locale).

get_locale() ->
    case get('l10n_locale') of
    'undefined' -> 'root';
    X -> X
    end.
    
delete_tids() ->
    [erase(X) || X <- get_tids()],
    ok.
    
add_domain(Name) ->
    Ds = get_domains(),
    case lists:any(fun(X) -> X=:=Name end, Ds) of
    false -> put('l10n_domains', [Name|Ds]);
    true  -> ok
    end.

get_domains() ->
    case get('l10n_domains') of
    'undefined' -> [];
    X -> X
    end.

get_tids() ->
    [X:get_name('table') || X <- get_domains()].
