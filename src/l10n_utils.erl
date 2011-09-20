-module(l10n_utils).
-export([hash/1, format/1, string/1]).

hash(X) -> erlang:phash2(X).
format(X) -> i18n_message:open(X).
string(X) -> i18n_string:from(X).
