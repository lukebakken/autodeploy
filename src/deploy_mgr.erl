-module(deploy_mgr).
-behavior(gen_server).

% client API
-export([start_link/0, stop/0, deploy/1, deploy_async/1]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

deploy(RepoData) ->
    gen_server:call(?MODULE, {deploy, RepoData}).

deploy_async(RepoData) ->
    gen_server:cast(?MODULE, {deploy, RepoData}).

init([]) ->
    lager:debug("deploy_mgr initialized", []),
    {ok, initialized}.

handle_call({deploy, RepoData}, _From, State) ->
    {ok, Reply} = do_deploy(RepoData),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({deploy, RepoData}, State) ->
    {ok, _Reply} = do_deploy(RepoData),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_deploy({RepoName, RepoFullName, RepoCloneUrl}) ->
    % Process:
    % Update git
    % if ok, restart monit
    {ok, MonitName, GitConfig} = config_util:git_config(RepoName, RepoFullName),
    lager:debug("monit name: ~p, git config ~p", [MonitName, GitConfig]),
    {ok, GitResult} = git_util:clone(RepoCloneUrl, GitConfig),
    lager:debug("git result: ~p", [GitResult]),
    MonitResult = [],
    % TODO {ok, MonitResult} = monit_util:service_action(restart, MonitName),
    % lager:debug("monit result: ~p", [MonitResult]),
    {ok, [{git, GitResult}, {monit, MonitResult}]}.
