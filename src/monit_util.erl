-module(monit_util).

-export([service_action/2]).

service_action(start, MonitName) ->
    do_service_action(MonitName, "start");
service_action(stop, MonitName) ->
    do_service_action(MonitName, "stop");
service_action(restart, MonitName) ->
    do_service_action(MonitName, "restart").

do_service_action(undefined, _Action) ->
    ResultMsg = "no monit name configured",
    ok = lager:info(ResultMsg),
    {ok, ResultMsg};
do_service_action(MonitName, Action) ->
    do_service_action_step2(MonitName, Action, config_util:monit_config()).

do_service_action_step2(MonitName, _Action, undefined) ->
    ResultMsg = io_lib:format("no global monit configuration for ~s", [MonitName]),
    ok = lager:info(ResultMsg),
    {ok, ResultMsg};
do_service_action_step2(MonitName, Action, {MonitUrl, MonitUser, MonitPass}) ->
    BasicAuthToken = base64:encode_to_string(string:join([MonitUser, MonitPass], ":")),
    BasicAuthHeader = string:concat("Basic ", BasicAuthToken),
    Method = post,
    URL = string:concat(MonitUrl, MonitName),
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

