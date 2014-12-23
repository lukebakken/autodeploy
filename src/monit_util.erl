-module(monit_util).

-export([service_action/2]).

service_action(start, ServiceName) ->
    do_service_action(ServiceName, "start");
service_action(stop, ServiceName) ->
    do_service_action(ServiceName, "stop");
service_action(restart, ServiceName) ->
    do_service_action(ServiceName, "restart").

do_service_action(ServiceName, Action) ->
    {MonitUrl, MonitUser, MonitPass} = config_util:monit_config(),
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
    case httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options) of
        {ok, ResultMsg} ->
            ok = lager:debug("httpc result message: ~p", [ResultMsg]),
            {ok, ResultMsg};
        {Err, ErrMsg} ->
            ok = lager:error("httpc error: ~p message: ~p", [Err, ErrMsg]),
            {ok, ErrMsg}
    end.
