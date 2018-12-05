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

to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) -> Value.
concat(List) ->
  list_to_binary(lists:flatten(lists:map(fun to_string/1, List))).

%% API

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

send_chat(Chatroom, Sender, Message) ->
  ebus:pub(Chatroom, {Sender, Message}).

websocket_init(_Type, Req, _Opts) ->
  % Create the handler from our custom callback
  Handler = ebus_proc:spawn_handler(fun chat_erlbus_handler:handle_msg/2, [self()]),
  ebus:sub(Handler, ?PUBLIC_CHATROOM),
  ebus:pub(?PUBLIC_CHATROOM, {<<"Server">>, concat([get_name(Req), " joined ", ?PUBLIC_CHATROOM])}),
  {ok, Req, #state{name = get_name(Req), handler = Handler, chatroom = ?PUBLIC_CHATROOM}, ?TIMEOUT}.

handle_chat(_, Req, State, [Type, Payload]) when Type =:= <<"name">> -> 
  send_chat(State#state.chatroom, <<"Server">>, concat([State#state.name, " changed name to ", Payload])),
  {ok, Req, #state{name = Payload, handler = State#state.handler, chatroom = State#state.chatroom}};
handle_chat(_, Req, State, [Type, Payload]) when Type =:= <<"chatroom">> -> 
  ebus:unsub(State#state.handler, State#state.chatroom),
  ebus:sub(State#state.handler, "chatroom_" ++ Payload),
  send_chat(State#state.chatroom, <<"Server">>, concat([State#state.name, " left this chatroom"])),
  send_chat(concat(["chatroom_", Payload]), <<"Server">>, concat([State#state.name, " joined ", concat(["chatroom_", Payload])])),
  {ok, Req, #state{name = State#state.name, handler = State#state.handler, chatroom = concat(["chatroom_", Payload])}};
handle_chat(Msg, Req, State, _) -> 
  send_chat(State#state.chatroom, State#state.name, Msg),
  {ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
  ebus:pub(?DEBUG, {text, Msg}),
  handle_chat(Msg, Req, State, string:split(Msg, "|"));
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({message_published, {Sender, Msg}}, Req, State) ->
  {reply, {text, jiffy:encode({[{sender, Sender}, {msg, Msg}]})}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
  % Unsubscribe the handler
  ebus:unsub(State#state.handler, State#state.chatroom),
  send_chat(State#state.chatroom, <<"Server">>, concat([State#state.name, " left this chatroom"])),
  ok.

%% Private methods

get_name(Req) ->
  {{Host, Port}, _} = cowboy_req:peer(Req),
  Name = list_to_binary(string:join([inet_parse:ntoa(Host), 
    ":", io_lib:format("~p", [Port])], "")),
  Name.
  