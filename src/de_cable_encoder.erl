-module(de_cable_encoder).
-include_lib("de_cable/include/de_cable.hrl").
-export([
  encode/2,
  decode/2
]).

encode(_, #de_cable_message{channel = Channel, data = Message}) ->
  Identifier = jsx:encode([{<<"channel">>, Channel}]),
    {text, jsx:encode([{<<"identifier">>, Identifier}, {<<"message">>, Message}])}.

decode(_, {text, Data}) ->
  Message = 
    case jsx:decode(Data, [return_maps]) of 
      #{<<"identifier">> := Identifier} = M ->
        M#{<<"identifier">> => jsx:decode(Identifier, [return_maps])};
      M ->
        M
    end,
  CableMessage = 
    case Message of
      #{<<"command">> := <<"subscribe">>, <<"identifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = subscribe, channel = Channel};
      #{<<"command">> := <<"unsubscribe">>, <<"identifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = unsubscribe, channel = Channel};
      #{<<"command">> := <<"message">>, <<"identifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = message, channel = Channel};
      _ ->
        #de_cable_message{}
    end,
  CableMessage#de_cable_message{data = maps:get(<<"data">>, Message, <<"">>)};

decode(_, {binary, _Data}) ->
  #de_cable_message{};

decode(_, _) ->
  #de_cable_message{}.