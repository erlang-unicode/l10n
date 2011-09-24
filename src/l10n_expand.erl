-module(l10n_expand).
-export([parse_transform/2]).

-import(ct_expand, [function/4]).

in(H, [H|_T]) -> true;
in(X, [_H|T]) -> in(X, T);
in(_X, []) -> false.

parse_transform(Forms, Options) ->
	try
	Info = erl_syntax_lib:analyze_forms(Forms),
	{'attributes', AttrList} = lists:keyfind('attributes', 1, Info),
	{'locale', Ds} = lists:keyfind('locale', 1, AttrList),
	io:format("Locale domains: ~w~n", [Ds]),
	Match = fun(M, F, A) ->
			in(M, Ds) andalso in(F, ['string', 'format']) andalso A > 0
		end,
	
    function(Match,
	     fun(Form, _Context) ->
		     case erl_syntax:application_arguments(Form) of
			 [H|T] = As ->
			     case erl_eval:exprs([H], []) of
				 {value, Value, _} ->
		     		io:format("expanding mes ~ts~n", [Value]),
					Oper = erl_syntax:application_operator(Form),
					Hash = l10n_utils:hash(Value),
					Args = [a(Hash), a(Value)|T],
				    erl_syntax:application(Oper, Args);
				 Other ->
				     erlang:error({cannot_evaluate,
						   [As, Other]})
			     end;
			 _Args ->
			     erlang:error(illegal_form)
		     end
	     end, Forms, Options)
	catch error:Reason -> 
		io:format("Cancel parse_transform: ~w ~n", [Reason]), 
		Forms
	end.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

a(X) ->
	erl_syntax:abstract(X).
