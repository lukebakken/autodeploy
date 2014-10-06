-module(monit_util).

-export([service_action/2]).

service_action(ServiceName, start) ->
    do_service_action(ServiceName, "start");
service_action(ServiceName, stop) ->
    do_service_action(ServiceName, "stop");
service_action(ServiceName, restart) ->
    do_service_action(ServiceName, "restart").

do_service_action(ServiceName, Action) ->
    {ok, MonitPath} = application:get_env(autodeploy, monit),
    {ok, RunResult} = exec:run([MonitPath, Action, ServiceName], [sync, stdout]),
    lager:info("restarted service ~p result: ~p", [ServiceName, RunResult]).

