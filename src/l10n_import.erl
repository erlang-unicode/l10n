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

% a lot of iolists

-module(l10n_import).
-export([from_po/1]).

-include("l10n_parser.hrl").


from_po(FileName) when is_list(FileName) ->
    {ok, Fd} = open_bom_file_for_reading(FileName),

    % Reader function
    F = get_reader(Fd),
    Res = do_read(F, #trans{}, []),
    file:close(Fd),
    Res.

do_read(F, X=#trans{}, Acc) 
    when is_function(F), 
         is_list(Acc) ->
% do_read(F, X, Acc)
    R = F(),
    case R of
    {ok, F2, "# " ++ Data} ->
        C = string:strip(Data, right, $\n),
        CC = X#trans.comment,
        NewX = X#trans{comment=[C|CC]},
        do_read(F2, NewX, Acc);
    {ok, F2, "msgid " ++ Data} ->
        {F3, Id} = extract_string(F2, [Data]),
        NewX = X#trans{id=Id},
        do_read(F3, NewX, Acc);
    {ok, F2, "msgstr " ++ Data} ->
        {F3, S} = extract_string(F2, [Data]),
        NewX = X#trans{string=S},
        do_read(F3, #trans{}, add(NewX, Acc));
    {ok, F2, _Wtf} -> % skip
        do_read(F2, X, Acc);

    eof -> lists:reverse(Acc)
    end.

%% @doc If X is valid, then merge it with Acc.
add(X=#trans{id=[_|_], comment=C}, Acc) ->
    NewC = lists:reverse(C),
    NewX = X#trans{comment=NewC}, 
    [NewX|Acc];
add(_X, Acc) ->
    Acc. % skip
    
extract_string(F, Acc) ->
    case F() of
    {ok, F2, [H|_] = Line} when H =:= $  orelse H =:= $" ->
        extract_string(F2, [Line|Acc]);
    X ->
        {proxy_reader(X), strip(lists:reverse(Acc))}
    end.

%% @doc Convert `["\"ff\"", "\"FF\"\n"]' to `"ffFF\n"'
strip(X) -> do_strip(X, []).

do_strip([H|T], Acc) ->
    NewAcc = do_unescape(H, Acc),
    do_strip(T, NewAcc);
do_strip([], Acc) ->
    lists:reverse(Acc).
    

%% Delete whitespaces and $" before the string
do_unescape([$ |T], Acc) ->
    do_unescape(T, Acc);
do_unescape([$"|T], Acc) ->
    do_unescape2(T, Acc);
do_unescape([], Acc) ->
    Acc.

do_unescape2([$\\, $\"|T], Acc) ->
    do_unescape2(T, [$\"|Acc]);
do_unescape2([$\n, $\n|T], Acc) ->
    do_unescape2(T, [$\n|Acc]);
do_unescape2([$\\, $\\|T], Acc) ->
    do_unescape2(T, [$\\|Acc]);
do_unescape2([$"|_T], Acc) ->
    Acc;
do_unescape2([H|T], Acc) ->
    do_unescape2(T, [H|Acc]).
    
    
        
    
%% @doc Return function: 
%%      fun() -> {ok, fun(), string()} | eof
get_reader(Fd) ->
    fun() -> 
        case file:read_line(Fd) of
        % Obsolete translation units
        {ok, "#~" ++ Line} ->
            {ok, get_reader(Fd), delete_prefix(Line)};
        {ok, Line} ->
            {ok, get_reader(Fd), Line};
        X -> 
            X
        end
    end.

delete_prefix(S) ->
    S1 = string:strip(S,  left, $\t),
         string:strip(S1, left, $ ).

proxy_reader(X) ->
    fun() -> X end.

    


open_bom_file_for_reading(File) ->
    {ok,F} = file:open(File,[read]),
    {ok,Bin} = file:read(F,4),
    {Type,Bytes} = unicode:bom_to_encoding(list_to_binary(Bin)),
    file:position(F,Bytes),

    io:setopts(F,[{encoding,Type}]),
    {ok,F}.
