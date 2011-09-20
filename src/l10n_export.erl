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

%%% Example:
%%%
%%% ```
%%% PO = l10n_import:from_po(code:priv_dir(l10n) ++ "/swedish.po").
%%% SRC = l10n_parser:parse_app(i18n_example).
%%% l10n_export:to_po(PO, SRC). '''

-module(l10n_export).
-export([to_pot/1, to_po/2]).

-include("l10n_parser.hrl").

%% String delimeter
-define(DEL, "\r\n").

%% Recommended max length of string (may be longer).
-define(STR_LEN, 70).


%% @doc Convert [#value2{}] to an iolist, which represents a pot file.
to_pot(Values) ->
	do_pot(Values, []).

fold(F, A, PO)
    when is_list(PO) ->
    do_fold(F, A, PO).

%% Merge data.
to_po(PO, SRC) 
    when is_list(SRC),
         is_list(PO) ->
	do_po(PO, SRC, []).




do_pot([H|T], Acc) ->
    NewAcc = append_untranslated(H, Acc),
	do_pot(T, NewAcc);
do_pot([], Acc) ->
	lists:reverse(Acc).

do_po([H=#trans{id=I}|T], SRC, Acc) ->
    SH = lists:keyfind(I, #value2.string, SRC),
    case SH of
    false ->
        NewAcc = append_deleted(H, Acc),
    	do_po(T, SRC, NewAcc);
    HS=#value2{} ->
        NewAcc = append_translated(H, HS, Acc),
        NewSRC = lists:keydelete(I, #value2.string, SRC),
    	do_po(T, NewSRC, NewAcc)
    end;
do_po([], [], Acc) ->
	lists:reverse(Acc);
do_po([], [_|_]=SRC, Acc) ->
    % Append new strings 
	do_pot(SRC, Acc). % yes, it is do_pot



do_fold(F, [H=#trans{id=I, string=[_|_]=S}|T], A) ->
    AA = F(A, I, S),
    do_fold(F, T, AA);
do_fold(F, [H=#trans{id=_I, string=[]}|T], A) ->
    do_fold(F, T, A);
do_fold(F, [], A) ->
    A.
    











%%
%% Common helpers
%%

do_comments([H|T], Acc) ->
	NewH = comment(H),
	NewAcc = [NewH|Acc],
	do_comments(T, NewAcc);
do_comments([], Acc) ->
	Acc.

do_user_comments([H|T], Acc) ->
	NewH = user_comment(H),
	NewAcc = [NewH|Acc],
	do_user_comments(T, NewAcc);
do_user_comments([], Acc) ->
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


do_obs_msgid(S, Acc) ->
	{H, T} = split(escape_chars(S), ?STR_LEN),
    Msg = ["#~ msgid \"", H, $", ?DEL],
	NewAcc = [Msg|Acc],
	do_obs_string(T, NewAcc).

do_obs_msgstr(S, Acc) ->
	{H, T} = split(escape_chars(S), ?STR_LEN),
    Msg = ["#~ msgstr \"", H, $", ?DEL],
	NewAcc = [Msg|Acc],
	do_obs_string(T, NewAcc).

do_obs_string([_|_]=S, Acc) ->
	{H, T} = split(S, ?STR_LEN),
	NewAcc = [["#~ \t\"", H, $"] | Acc],
	do_string(T, NewAcc);
do_obs_string([], Acc) ->
	Acc.

comment(S) ->
	["#. ", S, ?DEL].

user_comment(S) ->
	["# ", S, ?DEL].

place(S) ->
	["#: ", S, ?DEL].

split(Str, Max) ->
     do_split(Str, Max, [], [], []).


% Acc2 = Acc1 + Acc3 
do_split(T, Len, Acc1, _Acc2, Acc3) 
    when Len =< 0 ->
    {lists:reverse(Acc3), lists:reverse(Acc1, T)};
do_split([$ |T], Len, _Acc1, Acc2, _Acc3) ->
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



append_untranslated(#value2{string=I, place=P, comment=C}, Acc) ->
	Acc1 = do_comments(C, Acc),
	Acc2 = do_places(P, Acc1),
	Acc3 = do_msgid(I, Acc2),
	Acc4 = do_msgstr([], Acc3),
    [?DEL|Acc4].

append_deleted(#trans{id=I, string=S, comment=C}, Acc) ->
	Acc1 = do_user_comments(C, Acc),
	Acc2 = do_obs_msgid(I, Acc1),
	Acc3 = do_obs_msgstr(S, Acc2),
    [?DEL|Acc3].
        
append_translated(#trans{id=I, string=S, comment=U},
    #value2{place=P, comment=C}, Acc) ->
	Acc1 = do_user_comments(U, Acc),
	Acc2 = do_comments(C, Acc1),
    Acc3 = do_places(P, Acc2),
    Acc4 = do_msgid(I, Acc3),
    Acc5 = do_msgstr(S, Acc4),
    [?DEL|Acc5].


