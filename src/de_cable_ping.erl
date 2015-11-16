-module(de_cable_ping).
-behaviour(gen_fsm).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-include_lib("de_cable/include/de_cable.hrl").

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, ping/2, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3,
  code_change/4]).

-record(state, {
  channel = <<"">> ::binary(),
  timeout = 3000 ::non_neg_integer()
}).

%% API
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Channel = ?Config(ping_channel, <<"_ping">>),
  Timeout = ?Config(ping_timeout, 3000),
  {ok, ping, #state{channel = Channel, timeout = Timeout}, Timeout}.

ping(timeout, #state{channel = Channel, timeout = T} = State) ->
  Message = #de_cable_message{channel = Channel},
  Clients = deliverly:connections_list(de_cable),
  de_client:broadcast_to(Clients, Message),
  {next_state, ping, State, T}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, undefined, StateName, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
