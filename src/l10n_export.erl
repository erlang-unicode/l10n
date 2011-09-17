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

% a lot of iolists

-module(l10n_export).
-export([to_pot/1]).

-include("l10n_parser.hrl").

%% String delimeter
-define(DEL, "\r\n").

%% Recommended max length of string (may be longer).
-define(STR_LEN, 70).


%% @doc Convert [#value2{}] to an iolist, which represents a pot file.
to_pot(Values) ->
	do_pot(Values, []).

do_pot([H=#value2{string=S, place=P, comment=C}|T], Acc) ->
	Acc1 = do_comments(C, Acc),
	Acc2 = do_places(P, Acc1),
	Acc3 = do_msgid(S, Acc2),
	Acc4 = do_msgstr([], Acc3),
    Acc5 = [?DEL|Acc4],
	do_pot(T, Acc5);
do_pot([], Acc) ->
	lists:reverse(Acc).














%%
%% Common helpers
%%

do_comments([H|T], Acc) ->
	NewH = comment(H),
	NewAcc = [NewH|Acc],
	do_comments(T, NewAcc);
do_comments([], Acc) ->
	Acc.
	
do_places([H|T], Acc) ->
	NewH = place(H),
	NewAcc = [NewH|Acc],
	do_places(T, NewAcc);
do_places([], Acc) ->
	Acc.

do_msgid(S, Acc) ->
	{H, T} = split(escape_chars(S), ?STR_LEN),
    Msg = ["msgid \"", H, $", ?DEL],
	NewAcc = [Msg|Acc],
	do_string(T, NewAcc).

do_msgstr(S, Acc) ->
	{H, T} = split(escape_chars(S), ?STR_LEN),
    Msg = ["msgstr \"", H, $", ?DEL],
	NewAcc = [Msg|Acc],
	do_string(T, NewAcc).
    

do_string([_|_]=S, Acc) ->
	{H, T} = split(S, ?STR_LEN),
	NewAcc = [$", H, $", $\t | Acc],
	do_string(T, NewAcc);
do_string([], Acc) ->
	Acc.

comment(S) ->
	["#. ", S, ?DEL].

place(S) ->
	["#: ", S, ?DEL].

split(Str, Max) ->
     do_split(Str, Max, [], [], []).


% Acc2 = Acc1 + Acc3 
do_split(T, Len, Acc1, _Acc2, Acc3) 
    when Len =< 0 ->
    {lists:reverse(Acc3), lists:reverse(Acc1, T)};
do_split([$ |T], Len, Acc1, Acc2, Acc3) ->
    do_split(T, Len - 1, " ", [$ |Acc2], Acc2);
do_split([H|T], Len, Acc1, Acc2, Acc3) ->
    do_split(T, Len - 1, [H|Acc1], [H|Acc2], Acc3);
do_split([], _Len, _Acc1, Acc2, _Acc3) ->
    {lists:reverse(Acc2), []}.
    

escape_chars(Str) -> 
    do_escape(Str, []).

do_escape([H|T], Acc) -> 
    do_escape(T, escape_char(H, Acc));
do_escape([], Acc) -> 
    lists:reverse(Acc).
    

escape_char($",  Acc) -> [$",  $\\|Acc];
escape_char($\\, Acc) -> [$\\, $\\|Acc];
escape_char($\n, Acc) -> [$n,  $\\|Acc];
escape_char(C,   Acc) -> [C|Acc].


