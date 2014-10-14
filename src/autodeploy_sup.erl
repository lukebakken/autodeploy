-module(autodeploy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        {deploy_mgr, {deploy_mgr, start_link, []},
            permanent, 5, worker, [deploy_mgr]}
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
