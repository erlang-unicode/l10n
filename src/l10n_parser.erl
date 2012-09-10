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

-module(l10n_parser).
-export([parse/1]).

-include("l10n_parser.hrl").

-record(config, {domain, type}).

%% @doc M is instance of l10n_default.
parse(M) ->
    Files = M:source_files(),
    D = M:get_name('domain'),
    T = M:get_type(),
    C = #config{domain=D, type=T},
    parse_files(C, Files).




parse_files(C=#config{}, Files) ->
    F = fun(File, Acc) -> parse_file(C, File, Acc) end,
    merge(lists:foldl(F, [], Files)).

parse_file(C=#config{}, FileName, Acc) ->
    BaseName = filename:basename(FileName),
    Callses = parse_epp(C, FileName),
    Comments = parse_comments(FileName),

    F = fun(#l10n_call{line=Line, call=Call, string=Str}, L) ->
            Place = io_lib:format("~s:~B", [BaseName, Line]), 
            ComKey = lists:keyfind(Line, #comment.line, Comments),

            Comment = case ComKey of
                false -> [];
                #comment{} -> ComKey#comment.text
                end,

            H = #value{
                call    = Call,
                string  = Str, 
                place   = Place, 
                comment = Comment},

            [H|L]
        end,
    lists:foldl(F, Acc, Callses).
    
    

%% @doc Extract l10n_callses.
parse_epp(C=#config{}, FileName) ->
    {ok, Forms} = epp:parse_file(FileName, [], []),
    fold_epp(C, Forms).
    
fold_epp(#config{domain=Dom, type=Call}, Forms) ->
%{call,5,{remote,5,{atom,5,l10n_format},{atom,5,format}},[{atom,5,default},{string,5,[70]},{nil,5}]}
    F = fun({call,_Line,
                {remote,_Line,{atom,_Line,Dom2},{atom,_Line,Call2}},
                [{string,Line,Str}|_]}, L) 
             when Call =:= Call2 andalso Dom =:= Dom2 -> % Lets compiler be happy.
            H = #l10n_call{
                call   = Call,
                line   = Line, 
                string = lists:flatten(Str)}, % flatten, because:
                % We will search by `string' field in `l10n_export:to_po'
            [H|L];
            (_, L) -> L
        end,
    fold(F, [], Forms).




%% @doc Extract comments.
parse_comments(FileName) ->
    Parsed = erl_comment_scan:file(FileName),
    fold_comments(Parsed).

fold_comments(Comments) ->
    F = fun({Line, _Column, _Indentation, Text}, L) ->
            LineOfCall = Line + length(Text),
            H = #comment{
                    line = LineOfCall, 
                    text = Text},
            [H|L]
        end,
    lists:foldl(F, [], Comments).
        


%% @doc Delete repeated strings.
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

