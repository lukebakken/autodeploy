-module(config_util).

-export([monit_config/0, git_config/2, secret_token/1]).

monit_config() ->
    {ok, MonitConf} = application:get_env(autodeploy, monit),
    MonitUrl = proplists:get_value(url, MonitConf),
    MonitUser = proplists:get_value(user, MonitConf),
    MonitPass = proplists:get_value(pass, MonitConf),
    {MonitUrl, MonitUser, MonitPass}.

secret_token(RepoNameBin) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    RepoProperties = proplists:get_value(binary_to_list(RepoNameBin), Apps),
    EnvSecretVar = proplists:get_value(env_secret_var, RepoProperties),
    case os:getenv(EnvSecretVar) of
        false -> {error, "env variable not defined: " ++ EnvSecretVar};
        Value -> {ok, Value}
    end.

git_config(RepoName, RepoFullName) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    RepoProperties = proplists:get_value(RepoName, Apps),
    RepoFullNameCheckFun = fun(E) -> E =:= {full_name, RepoFullName} end,
    case lists:any(RepoFullNameCheckFun, RepoProperties) of
        true ->
            GitRepoPath = proplists:get_value(clone_path, RepoProperties),
            GitRepoUser = proplists:get_value(user, RepoProperties),
            GitRepoGroup = proplists:get_value(group, RepoProperties),
            MonitName = proplists:get_value(monit_name, RepoProperties),
            {ok, MonitName, {GitRepoPath, GitRepoUser, GitRepoGroup}};
        false ->
            {error, "No config for repo with name " ++ RepoName ++
                    " full name " ++ RepoFullName}
    end.

