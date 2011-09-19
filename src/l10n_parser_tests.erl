-module(l10n_parser_tests).
-compile([export_all]).

example() ->
	l10n_format:format('default', "F", []).

test() ->
	M = l10n_default_app:new(l10n),
	l10n_parser:parse(M).

