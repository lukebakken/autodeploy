-module(autodeploy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    lager:start(),
    Dispatch = cowboy_router:compile([
        {'_', [{"/autodeploy", autodeploy_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    autodeploy_sup:start_link().

stop(_State) ->
	ok.
