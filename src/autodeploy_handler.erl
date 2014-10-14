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
            {ok, Body, Req2} = cowboy_req:body(Req),
            {RepoName, RepoFullName} = process_postreq_body(Body),
            lager:debug("autodeploy_handler|RepoName: ~p, RepoFullName: ~p",
                        [RepoName, RepoFullName]),
            deploy_mgr:deploy_async(RepoName, RepoFullName),
            {ok, ReqRsp} = cowboy_req:reply(204, Req2);
        false ->
            {ok, ReqRsp} = cowboy_req:reply(400, Req)
    end,
    {ok, ReqRsp, State};
do_handle({_, Req}, State) ->
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello From Autodeploy!">>,
        Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

process_postreq_body(Body) ->
    {struct, JsonData} = mochijson2:decode(Body),
    {struct, RepoData} = proplists:get_value(<<"repository">>, JsonData),
    RepoName = binary_to_list(proplists:get_value(<<"name">>, RepoData)),
    RepoFullName = binary_to_list(proplists:get_value(<<"full_name">>, RepoData)),
    {RepoName, RepoFullName}.
