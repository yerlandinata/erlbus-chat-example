-module(play).

-export([update_state/2]).
-export([testing/4]).

-define(DEBUG, "debug").

-record(state, {name, handler, chatroom}).

update_state(State, [Type, Payload]) when Type =:= "name" -> 
  ebus:pub(?DEBUG, "Name change from " ++ State#state.name ++ " to " ++ Payload),
  #state{name = Payload, handler = State#state.handler, chatroom = State#state.chatroom};
update_state(State, [Type, Payload]) when Type =:= "chatroom" -> 
  ebus:pub(?DEBUG, State#state.name ++ " left " ++ State#state.chatroom ++ " and then joined chatroom_" ++ Payload),
  ebus:unsub(State#state.handler, State#state.chatroom),
  ebus:sub(State#state.handler, "chatroom_" ++ Payload),
  #state{name = State#state.name, handler = State#state.handler, chatroom = "chatroom_" ++ Payload};
update_state(State, _) -> 
  ebus:pub(?DEBUG, "aa message has been sent"),
  State.

testing(Name, Chatroom, Handler, Message) -> update_state(
    #state{name = Name, handler = Handler, chatroom = Chatroom},
    string:split(Message, "|")
).
