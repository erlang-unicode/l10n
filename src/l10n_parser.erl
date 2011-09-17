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

-module(l10n_parser).
-export([parse_file/1]).
-export([parse_app/0, parse_app/1]).

-include("l10n_parser.hrl").

fold(F, Acc, [H|T]) 
    when is_tuple(H) ->
    Acc1 = F(H, Acc),

    List = tuple_to_list(H),
    Acc2 = fold(F, Acc1, List),

    fold(F, Acc2, T);

fold(F, Acc, [H|T]) 
    when is_list(H) ->
    Acc1 = F(H, Acc),

    Acc2 = fold(F, Acc1, H),

    fold(F, Acc2, T);

fold(F, Acc, [H|T]) ->
    Acc1 = F(H, Acc),
    fold(F, Acc1, T);

fold(_F, Acc, []) ->
    Acc.

parse_app() ->
    Name = application:get_application(),
    parse_app(Name).

parse_app(Name) ->
    DirName = code:lib_dir(Name, src),
    parse_dir(DirName).

parse_dir([_|_] = DirName) ->
    Files = filelib:wildcard(DirName ++ "/*.erl"),
    merge(parse_files(Files)).


parse_files(Files) ->
    F = fun parse_file/2,
    lists:foldl(F, [], Files).

parse_file(FileName) ->
    merge(parse_file(FileName, [])).

parse_file(FileName, Acc) ->
    BaseName = filename:basename(FileName),
    M = parse_epp(FileName),
    C = parse_comments(FileName),

    F = fun(#macro{line=Line, macro=Macro, string=Str}, L) ->
            Place = io_lib:format("~s:~B", [BaseName, Line]), 
            ComKey = lists:keyfind(Line, #comment.line, C),

            Comment = case ComKey of
                false -> [];
                #comment{} -> ComKey#comment.text
                end,

            H = #value{
                macro   = Macro,
                string  = Str, 
                place   = Place, 
                comment = Comment},

            [H|L]
        end,
    lists:foldl(F, Acc, M).
    
    

parse_epp(FileName) ->
    {ok, Forms} = epp_dodger:parse_file(FileName),
    fold_epp(Forms).
    
fold_epp(Forms) ->
    F = fun({macro,{var,Line,Macro},[{string,_Line,Str} | _]}, L) 
            when Macro =:= 'LSTR' orelse Macro =:= 'LFMT' ->
            H = #macro{
                macro  = Macro, 
                line   = Line, 
                string = Str},
            [H|L];
            (_, L) -> L
        end,
    fold(F, [], Forms).


parse_comments(FileName) ->
    Parsed = erl_comment_scan:file(FileName),
    fold_comments(Parsed).

fold_comments(Comments) ->
    F = fun({Line, _Column, _Indentation, Text}, L) ->
            LineOfMacro = Line + length(Text),
            H = #comment{
                    line = LineOfMacro, 
                    text = Text},
            [H|L]
        end,
    lists:foldl(F, [], Comments).
        


merge(Values) ->
    do_merge(Values, []).

do_merge([#value{place=P, comment=C, string=S} | T], Acc) ->
    StrKey = lists:keyfind(S, #value2.string, Acc),
    NewAcc = case StrKey of
        false -> 
            NewH = #value2{place=[P], comment=C, string=S},
            [NewH|Acc];
        HH=#value2{place=PP, comment=CC} -> 
            NewP = [P|PP],
            NewC = CC ++ C, % quadric
            NewH = HH#value2{place=NewP, comment=NewC},
            lists:keyreplace(S, #value2.string, Acc, NewH)
        end,
    do_merge(T, NewAcc);
do_merge([], Acc) -> do_reverse(Acc, []).
    
do_reverse([H=#value2{place=P} | T], Acc) ->
    NewP = lists:reverse(P),
    NewH = H#value2{place=NewP},
    NewAcc = [NewH|Acc],
    do_reverse(T, NewAcc);
do_reverse([], Acc) -> Acc.
