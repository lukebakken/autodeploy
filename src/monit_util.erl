-module(monit_util).

-export([service_action/2]).

service_action(ServiceName, start) ->
    do_service_action(ServiceName, "start");
service_action(ServiceName, stop) ->
    do_service_action(ServiceName, "stop");
service_action(ServiceName, restart) ->
    do_service_action(ServiceName, "restart").

do_service_action(ServiceName, Action) ->
    {MonitUrl, MonitUser, MonitPass} = get_monit_conf(),
    BasicAuthToken = base64:encode_to_string(string:join([MonitUser, MonitPass], ":")),
    BasicAuthHeader = string:concat("Basic ", BasicAuthToken),
    Method = post,
    URL = string:concat(MonitUrl, ServiceName),
    Header = [
        {"Authorization", BasicAuthHeader}
    ],
    Type = "application/x-www-form-urlencoded",
    Body = string:concat("action=", Action),
    HTTPOptions = [],
    Options = [],
    {ok, R} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    lager:debug("monit http response: ~p", [R]),
    ok.

get_monit_conf() ->
    {ok, MonitConf} = application:get_env(autodeploy, monit),
    MonitUrl = proplists:get_value(url, MonitConf),
    MonitUser = proplists:get_value(user, MonitConf),
    MonitPass = proplists:get_value(pass, MonitConf),
    {MonitUrl, MonitUser, MonitPass}.
