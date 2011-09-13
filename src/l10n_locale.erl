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

-module(l10n_locale).
-export([set_locale/1, get_locale/0]).

-define(SERVER, 'l10n_locale_server').

set_locale(Value) -> 
    LName = i18n_locale:base_name(Value),
    erlang:put('l10n_locale', LName),
    LName.

get_locale() ->
    case erlang:get('l10n_locale') of
    'undefined' ->
        % Get a global value
        Value = i18n_locale:get_locale(),
        set_locale(Value);
    Value ->
        Value
    end.
