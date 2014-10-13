-module(config_util).

-export([config_for/2]).

config_for(RepoName, RepoFullName) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    RepoProperties = proplists:get_value(RepoName, Apps),
    RepoFullNameCheckFun = fun(E) -> E =:= {full_name, RepoFullName} end,
    case lists:any(RepoFullNameCheckFun , RepoProperties) of
        true ->
            GitRepoPath = proplists:get_value(clone_path, RepoProperties),
            GitRepoUser = proplists:get_value(user, RepoProperties),
            MonitName = proplists:get_value(monit_name, RepoProperties),
            {ok, GitRepoPath, GitRepoUser, MonitName};
        false ->
            {error, "No config for repo with name " ++ RepoName ++
                    " full name " ++ RepoFullName}
    end.

