-module(autodeploy_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    do_handle(cowboy_req:method(Req), State).

do_handle({<<"POST">>, Req}, State) ->
    lager:debug("POST request: ~p", [Req]),
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Req2} = cowboy_req:reply(204, Req);
        false ->
            {ok, Req2} = cowboy_req:reply(400, Req)
    end,
    {ok, Req2, State};
do_handle({_, Req}, State) ->
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello From Autodeploy!">>,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
