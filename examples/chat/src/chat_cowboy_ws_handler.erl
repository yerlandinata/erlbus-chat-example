-module(chat_cowboy_ws_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(PUBLIC_CHATROOM, "chatroom_public").
-define(TIMEOUT, 5 * 60 * 1000). % Innactivity Timeout
-define(DEBUG, "debug").

-record(state, {name, handler, chatroom}).

%% API

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
  % Create the handler from our custom callback
  Handler = ebus_proc:spawn_handler(fun chat_erlbus_handler:handle_msg/2, [self()]),
  ebus:sub(Handler, ?PUBLIC_CHATROOM),
  {ok, Req, #state{name = get_name(Req), handler = Handler, chatroom = ?PUBLIC_CHATROOM}, ?TIMEOUT}.

update_state(State, [Type, Payload]) when Type =:= <<"name">> -> 
  % ebus:pub(?DEBUG, "Name change from " ++ State#state.name ++ " to " ++ Payload),
  #state{name = Payload, handler = State#state.handler, chatroom = State#state.chatroom};
update_state(State, [Type, Payload]) when Type =:= <<"chatroom">> -> 
  % ebus:pub(?DEBUG, State#state.name ++ " left " ++ State#state.chatroom ++ " and then joined chatroom_" ++ Payload),
  ebus:unsub(State#state.handler, State#state.chatroom),
  ebus:sub(State#state.handler, "chatroom_" ++ Payload),
  #state{name = State#state.name, handler = State#state.handler, chatroom = "chatroom_" ++ Payload};
update_state(State, _) -> 
  ebus:pub(?DEBUG, "aa message has been sent"),
  State.

websocket_handle({text, Msg}, Req, State) ->
  ebus:pub(State#state.chatroom, {State#state.name, Msg}),
  ebus:pub(?DEBUG, string:split(Msg, "|")),
  {ok, Req, update_state(State, string:split(Msg, "|"))};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({message_published, {Sender, Msg}}, Req, State) ->
  {reply, {text, jiffy:encode({[{sender, Sender}, {msg, Msg}]})}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
  % Unsubscribe the handler
  ebus:unsub(State#state.handler, State#state.chatroom),
  ok.

%% Private methods

get_name(Req) ->
  {{Host, Port}, _} = cowboy_req:peer(Req),
  Name = list_to_binary(string:join([inet_parse:ntoa(Host), 
    ":", io_lib:format("~p", [Port])], "")),
  Name.
  