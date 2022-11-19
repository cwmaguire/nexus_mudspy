-module(achaea_websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-define(HOUR, 60 * 60 * 1000).

init(Req, State) ->
    io:format(user, "Websocket handler init: Req =~n~p~n", [Req]),
    {cowboy_websocket, Req, State, #{idle_timeout => 8 * ?HOUR}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Text}, State) ->
    io:format(user, "Handle: Text = ~p~n", [Text]),
    {ok, State};
websocket_handle({binary, Bin = <<Sample:20/binary, _/binary>>}, State) ->
    io:format(user, "Handle: Bin sample = ~p~n", [Sample]),
    achaea_server:update(Bin),
    {ok, State};
websocket_handle({binary, ShortBin}, State) ->
    io:format(user, "Handle: ShortBin = ~p~n", [ShortBin]),
    achaea_server:update(ShortBin),
    {ok, State};
websocket_handle(Other, State) ->
    io:format(user, "Handle: Other = ~p~n", [Other]),
    {ok, State}.

websocket_info(Info, State) ->
    io:format(user, "Ignored Info = ~p~n", [Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    io:format(user, "Terminating with reason: ~p~n", [Reason]),
    ok.

