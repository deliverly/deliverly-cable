-module(de_cable_server).
-behaviour(gen_server).
-behaviour(deliverly_handler).
-include_lib("de_cable/include/de_cable.hrl").
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("deliverly/include/log.hrl").
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/0]).
%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

-record(state, {
  channels = #{} :: #{binary() | pid() => pid() | binary()}
}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  ?I("Starting application: de_cable"),
  {ok, #state{}}.

authorize(Client, Data) ->
  NewData = 
    case is_binary(Data) of
      true -> Data;
      false -> <<"">>
    end,
  case deliverly_utils:auth_from_config(cable, Client, NewData) of
    true -> {ok, Client#de_client{data = #{token => NewData, channels => []}, encoder = de_cable_encoder}};
    false -> {error, 3401}
  end.

handle_message(_, _) -> 
  ok.

handle_client_message(_Client, #de_cable_message{command = undefined}) -> 
  ok;

handle_client_message(Client, #de_cable_message{command = Command} = Message) -> 
  gen_server:call(?SERVER, {Command, Client, Message});

handle_client_message(_, _) -> 
  ok.

client_disconnected(Client) -> 
  gen_server:call(?SERVER, {disconnected, Client}).

handle_call({subscribe, Client, #de_cable_message{channel = Channel, data = Data}}, _, #state{channels = Channels} = State) ->
  {ChannelPid, NewChannels} = get_channel(Channel, Channels),
  Reply = de_cable_channel:add(ChannelPid, Client, Data),
  {reply, Reply, State#state{channels = NewChannels}};

handle_call({unsubscribe, Client, #de_cable_message{channel = Channel, data = Data}}, _, #state{channels = Channels} = State) ->
  {ChannelPid, NewChannels} = get_channel(Channel, Channels),
  Reply = de_cable_channel:remove(ChannelPid, Client, Data),
  {reply, Reply, State#state{channels = NewChannels}};

handle_call({message, Client, #de_cable_message{}}, _, State) ->
  {reply, {ok, Client}, State};

handle_call({disconnected, _Client}, _, State) ->
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{channels = Channels} = State) ->
  {noreply, State#state{channels = remove_channel(Pid, Channels)}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%private

get_channel(Channel, Channels) ->
  case Channels of
    #{Channel := Pid} ->
      {Pid, Channels}; 
    #{} ->
      {ok, Pid} = de_cable_sup:start_channel(Channel),
      erlang:monitor(process, Pid),
      {Pid, Channels#{Channel => Pid, Channel => Pid}}
  end.  

remove_channel(Pid, Channels) ->
  case Channels of
    #{Pid := Channel} ->
      maps:without([Pid, Channel]); 
    #{} ->
      Channels
  end.  
