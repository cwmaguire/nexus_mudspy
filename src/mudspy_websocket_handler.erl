-module(mudspy_websocket_handler).

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

websocket_handle({text, <<"register: ", Type/binary>>}, State) ->
    io:format(user, "Register ~p for ~p~n", [self(), Type]),
    achaea_server:register(Type),
    {ok, State};
websocket_handle({text, "unregister: " ++ Type}, State) ->
    io:format(user, "Register ~p for ~p~n", [self(), Type]),
    achaea_server:register(Type),
    {ok, State};
websocket_handle(InFrame, State) ->
    io:format(user, "Handle: InFrame = ~p~n", [InFrame]),
    {ok, State}.


websocket_info({send, Bin}, State) ->
    Sample = case Bin of
                 <<First:20/binary, _/binary>> ->
                     <<First/binary, " ...">>;
                 Short ->
                     Short
             end,

    io:format(user, "Sending update to ~p~n~p~n", [self(), Sample]),
    {reply, {binary, Bin}, State};
websocket_info(Info, State) ->
    io:format(user, "Ignored Info = ~p~n", [Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    io:format(user, "Terminating with reason: ~p~n", [Reason]),
    achaea_server:unregister(),
    ok.
