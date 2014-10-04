-module(monit_util).

-export([restart_service/1]).

restart_service(ServiceName) ->
    {ok, MonitPath} = application:get_env(autodeploy, monit),
    {ok, RunResult} = exec:run([MonitPath, "restart", ServiceName], [sync, stdout]),
    lager:info("restarted service ~p result: ~p", [ServiceName, RunResult]).
