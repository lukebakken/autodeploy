-module(deploy_mgr).
-behavior(gen_server).

% client API
-export([start_link/0, stop/0, deploy/2, deploy_async/2]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

deploy(RepoName, RepoFullName) ->
    gen_server:call(?MODULE, {deploy, RepoName, RepoFullName}).

deploy_async(RepoName, RepoFullName) ->
    gen_server:cast(?MODULE, {deploy, RepoName, RepoFullName}).

init([]) ->
    lager:debug("deploy_mgr initialized", []),
    {ok, initialized}.

handle_call({deploy, RepoName, RepoFullName}, _From, State) ->
    {ok, Reply} = do_deploy(RepoName, RepoFullName),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({deploy, RepoName, RepoFullName}, State) ->
    {ok, _Reply} = do_deploy(RepoName, RepoFullName),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_deploy(RepoName, RepoFullName) ->
    % Process:
    % Update git
    % if ok, restart monit
    {ok, GitRepoPath, GitRepoUser, MonitName} =
        config_util:git_config(RepoName, RepoFullName),
    lager:debug("git repo: ~p, user: ~p, monit name: ~p",
                [GitRepoPath, GitRepoUser, MonitName]),
    {ok, GitResult} = git_util:pull(GitRepoPath, GitRepoUser),
    lager:debug("git result: ~p", [GitResult]),
    {ok, MonitResult} = monit_util:service_action(restart, MonitName),
    lager:debug("monit result: ~p", [MonitResult]),
    {ok, [{git, GitResult}, {monit, MonitResult}]}.
