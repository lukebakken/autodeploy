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
    ok = lager:debug("POST repo ~p request: ~p", [RepoNameBin, Req2]),
    {ok, ReqRsp} = case cowboy_req:has_body(Req2) of
        true ->
            process_postreq(RepoNameBin, Req2);
        false ->
            cowboy_req:reply(400, Req2)
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
    MaybeSecretToken = config_util:secret_token(RepoNameBin),
    ValidationResult = validate_secret(MaybeSecretToken, XHubHeaderSignatureBin, RepoNameBin, Body),
    process_postreq_step2(ValidationResult, Req3).

process_postreq_step2({ok, Body}, Req) ->
    {XHubEventBin, Req2} = cowboy_req:header(<<"x-github-event">>, Req),
    ok = lager:debug("x-github-event header: ~p", [XHubEventBin]),
    process_postreq_step3(XHubEventBin, Body, Req2);
process_postreq_step2(error, Req) ->
    cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], <<"invalid signature token">>, Req).

process_postreq_step3(<<"ping">>, _Body, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"pong">>, Req);
process_postreq_step3(<<"push">>, Body, Req) ->
    RepoData = process_postreq_body(Body),
    ok = lager:debug("autodeploy_handler|RepoData: ~p", [RepoData]),
    deploy_mgr:deploy_async(RepoData),
    cowboy_req:reply(204, Req);
process_postreq_step3(UnknownEvent, _Body, Req) ->
    ok = lager:error("unknown github event: ~p", [UnknownEvent]),
    cowboy_req:reply(500, [{<<"content-type">>, <<"text/plain">>}], <<"unknown github event">>, Req).

validate_secret(false, _XHubHeaderSignatureBin, RepoNameBin, Body) ->
    ok = lager:warning("no secret token for git repo ~s", [RepoNameBin]),
    {ok, Body};
validate_secret(SecretToken, XHubHeaderSignatureBin, _RepoNameBin, Body) ->
    %% http://stackoverflow.com/questions/4193543/erlang-calculating-hmac-sha1-example
    <<Mac:160/integer>> = crypto:hmac(sha, SecretToken, Body),
    CalculatedSignature = "sha1=" ++ lists:flatten(io_lib:format("~40.16.0b", [Mac])),
    XHubHeaderSignature = binary_to_list(XHubHeaderSignatureBin),
    ok = lager:debug("validate_secret header: ~p calculated: ~p", [XHubHeaderSignature, CalculatedSignature]),
    case string:equal(XHubHeaderSignature, CalculatedSignature) of
        true -> {ok, Body};
        false -> error
    end.

process_postreq_body(Body) ->
    {struct, JsonData} = mochijson2:decode(Body),
    Ref = binary_to_list(proplists:get_value(<<"ref">>, JsonData)),
    {struct, RepoData} = proplists:get_value(<<"repository">>, JsonData),
    RepoName = binary_to_list(proplists:get_value(<<"name">>, RepoData)),
    RepoFullName = binary_to_list(proplists:get_value(<<"full_name">>, RepoData)),
    RepoCloneUrl = binary_to_list(proplists:get_value(<<"ssh_url">>, RepoData)),
    {Ref, RepoName, RepoFullName, RepoCloneUrl}.
