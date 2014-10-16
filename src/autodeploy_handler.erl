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
    {RepoNameBin, Req2} = cowboy_req:binding(reponame, Req),
    lager:debug("POST repo ~p request: ~p", [RepoNameBin, Req2]),
    case cowboy_req:has_body(Req2) of
        true ->
            {ok, ReqRsp} = process_postreq(RepoNameBin, Req2);
        false ->
            {ok, ReqRsp} = cowboy_req:reply(400, Req2)
    end,
    {ok, ReqRsp, State};
do_handle({_, Req}, State) ->
    {ok, ReqRsp} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello From Autodeploy!">>, Req),
    {ok, ReqRsp, State}.

terminate(_Reason, _Req, _State) ->
	ok.

process_postreq(RepoNameBin, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {XHubHeaderSignatureBin, Req3} = cowboy_req:header(<<"x-hub-signature">>, Req2),
    case validate_secret(XHubHeaderSignatureBin, RepoNameBin, Body) of
        ok ->
            RepoData = process_postreq_body(Body),
            lager:debug("autodeploy_handler|RepoData: ~p", [RepoData]),
            deploy_mgr:deploy_async(RepoData),
            {ok, _ReqRsp} = cowboy_req:reply(204, Req3);
        error ->
            {ok, _ReqRsp} = cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], <<"invalid signature token">>, Req3)
    end.

validate_secret(XHubHeaderSignatureBin, RepoNameBin, Body) ->
    %% signature = 'sha1=' + OpenSSL::HMAC.hexdigest(OpenSSL::Digest.new('sha1'), ENV['SECRET_TOKEN'], payload_body)
    %% http://stackoverflow.com/questions/4193543/erlang-calculating-hmac-sha1-example
    {ok, SecretToken} = config_util:secret_token(RepoNameBin),
    <<Mac:160/integer>> = crypto:hmac(sha, SecretToken, Body),
    CalculatedSignature = "sha1=" ++ lists:flatten(io_lib:format("~40.16.0b", [Mac])),
    XHubHeaderSignature = binary_to_list(XHubHeaderSignatureBin),
    lager:debug("validate_secret header: ~p calculated: ~p", [XHubHeaderSignature, CalculatedSignature]),
    case string:equal(XHubHeaderSignature, CalculatedSignature) of
        true -> ok;
        false -> error
    end.

process_postreq_body(Body) ->
    {struct, JsonData} = mochijson2:decode(Body),
    {struct, RepoData} = proplists:get_value(<<"repository">>, JsonData),
    RepoName = binary_to_list(proplists:get_value(<<"name">>, RepoData)),
    RepoFullName = binary_to_list(proplists:get_value(<<"full_name">>, RepoData)),
    RepoCloneUrl = binary_to_list(proplists:get_value(<<"ssh_url">>, RepoData)),
    {RepoName, RepoFullName, RepoCloneUrl}.
