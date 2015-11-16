-module(de_cable_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_channel/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel(Channel) ->
  supervisor:start_child(channel_sup, [Channel]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([channel_sup]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {undefined, {de_cable_channel, start_link, []},
      temporary, 2000, worker, [de_cable_channel]}
  ]}};

init([]) ->
  Children = [
    {
      channel_sup, 
      {
      supervisor,
      start_link,
      [{local, channel_sup}, ?MODULE, [channel_sup]]
    },
    permanent,
    infinity,
    supervisor,
    []
  },
    ?CHILD(de_cable_server, worker)
  ],
  {ok, { {one_for_one, 5, 10}, Children} }.
