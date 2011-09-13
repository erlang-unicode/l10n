%% @doc l10n.
%% @private

-module(l10n).
-export([start/0, stop/0]).
-export([repeat/2]).

%% @spec start() -> ok
%% @doc Start the ux server.
start() ->
    application:start(l10n).

repeat(N, F) when N>0 -> 
F(), repeat(N-1, F);
repeat(N, F) -> ok.

%% @spec stop() -> ok
%% @doc Stop the ux server.
stop() ->
    application:stop(l10n).
