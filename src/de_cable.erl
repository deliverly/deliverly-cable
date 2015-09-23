-module(de_cable).


%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, upgrade/0]).
-export([deliverly_handler/0]).

start() ->
    application:start(de_cable).

stop() ->
    application:stop(de_cable).

upgrade() ->
    ulitos_app:reload(de_cable).

deliverly_handler() -> de_cable_server.