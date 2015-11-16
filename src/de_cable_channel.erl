-module(de_cable_channel).
-behaviour(gen_server).
-include_lib("de_cable/include/de_cable.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/1]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%% API
-export([add/3, remove/3, disconnected/2]).

-record(state, {
  channel = <<"">> :: binary(),
  clients = [] :: [pid()]
}).

start_link(Channel) ->
  gen_server:start_link(?MODULE, [Channel], []).

add(Pid, Client, Data) ->
  gen_server:call(Pid, {add, Client, Data}).

remove(Pid, Client, Data) ->
  gen_server:call(Pid, {remove, Client, Data}).

disconnected(Pid, Client) ->
  gen_server:call(Pid, {disconnected, Client}).

init([Channel]) ->
  {ok, #state{channel = Channel}}.

handle_call({add, #de_client{data = #{channels := Channels}} = Client, _Data}, _, State) ->
  {reply, ok, State};

handle_call({remove, #de_client{data = #{channels := Channels}}, _Data}, _, State) ->
  {reply, ok, State};

handle_call({disconnected, Client}, _, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.