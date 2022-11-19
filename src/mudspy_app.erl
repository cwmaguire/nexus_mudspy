%%%-------------------------------------------------------------------
%% @doc webserver public API
%% @end
%%%-------------------------------------------------------------------

-module(mudspy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/achaea", achaea_websocket_handler, []},
               {"/mudspy", mudspy_websocket_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(mudspy,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    mudspy_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
