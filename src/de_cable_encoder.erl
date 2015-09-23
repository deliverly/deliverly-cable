-module(de_cable_encoder).
-include_lib("de_cable/include/de_cable.hrl").
-export([
  encode/2,
  decode/2
]).

encode(_, #de_cable_message{channel = Channel, data = Message}) ->
  Indentifier = jsx:encode([{<<"channel">>, Channel}]),
    {text, jsx:encode([{<<"indentifier">>, Indentifier}, {<<"message">>, Message}])}.

decode(_, {text, Data}) ->
  Message = 
    case jsx:decode(Data, [return_maps]) of 
      #{<<"indentifier">> := Indentifier} = M ->
        M#{<<"indentifier">> => jsx:decode(Indentifier, [return_maps])};
      M ->
        M
    end,
  CableMessage = 
    case Message of
      #{<<"command">> := <<"subscribe">>, <<"indentifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = subscribe, channel = Channel};
      #{<<"command">> := <<"unsubscribe">>, <<"indentifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = unsubscribe, channel = Channel};
      #{<<"command">> := <<"message">>, <<"indentifier">> := #{<<"channel">> := Channel}} ->
        #de_cable_message{command = message, channel = Channel};
      _ ->
        #de_cable_message{}
    end,
  CableMessage#de_cable_message{data = maps:get(<<"data">>, Message, <<"">>)};

decode(_, {binary, _Data}) ->
  #de_cable_message{};

decode(_, _) ->
  #de_cable_message{}.