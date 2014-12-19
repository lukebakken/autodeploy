-module(config_util).

-export([monit_config/0, git_config/3, secret_token/1]).

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

first_config(Ref, RepoName, RepoFullName, []) ->
    {error, "No config for repo with ref " ++ Ref ++
        ", name " ++ RepoName ++
        ", full name " ++ RepoFullName};
first_config(Ref, RepoName, RepoFullName, [{RepoName, RepoProperties} | Rest]) ->
    case lists:member({full_name, RepoFullName}, RepoProperties) andalso
         lists:member({ref, Ref}, RepoProperties) of
        true ->
            GitRepoPath = proplists:get_value(clone_path, RepoProperties),
            GitRepoUser = proplists:get_value(user, RepoProperties),
            GitRepoGroup = proplists:get_value(group, RepoProperties),
            MonitName = proplists:get_value(monit_name, RepoProperties),
            {ok, MonitName, {GitRepoPath, GitRepoUser, GitRepoGroup}};
        false ->
            first_config(Ref, RepoName, RepoFullName, Rest)
    end;
first_config(Ref, RepoName, RepoFullName, [ _WrongRepo | Rest]) ->
    first_config(Ref, RepoName, RepoFullName, Rest);
first_config(_,_,_,Config) ->
    {error, lists:flatten(io_lib:format("Invalid configuration: {apps,~p}",[Config]))}.

git_config(Ref, RepoName, RepoFullName) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    first_config(Ref, RepoName, RepoFullName, Apps).

