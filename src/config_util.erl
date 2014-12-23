-module(config_util).
-include("spec_types.hrl").

-export([monit_config/0, git_config/3, secret_token/1]).

monit_config() ->
    MonitConf = application:get_env(autodeploy, monit, []),
    process_monit_config(MonitConf).

process_monit_config([]) ->
    undefined;
process_monit_config(MonitConf) ->
    MonitUrl = proplists:get_value(url, MonitConf),
    MonitUser = proplists:get_value(user, MonitConf),
    MonitPass = proplists:get_value(pass, MonitConf),
    {MonitUrl, MonitUser, MonitPass}.

-spec secret_token(RepoNameBin) -> RV when
      RepoNameBin :: repo_name(),
      RV :: false | string().
secret_token(RepoNameBin) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    RepoProperties = proplists:get_value(binary_to_list(RepoNameBin), Apps),
    EnvSecretVar = proplists:get_value(env_secret_var, RepoProperties),
    os:getenv(EnvSecretVar).

git_config(Ref, RepoName, RepoFullName) ->
    {ok, Apps} = application:get_env(autodeploy, apps),
    RefStr = binary_to_list(Ref),
    RepoNameStr = binary_to_list(RepoName),
    RepoFullNameStr = binary_to_list(RepoFullName),
    first_config(RefStr, RepoNameStr, RepoFullNameStr, Apps).

first_config(Ref, RepoName, RepoFullName, []) ->
    ErrMsg = io_lib:format("No config for repo with ref ~s, name ~s, full name ~s", [Ref, RepoName, RepoFullName]),
    {error, ErrMsg};
first_config(Ref, RepoName, RepoFullName, [{RepoName, RepoProperties} | Rest]) ->
    case lists:member({full_name, RepoFullName}, RepoProperties) andalso
         lists:member({ref, Ref}, RepoProperties) of
        true ->
            GitRepoPath = proplists:get_value(clone_path, RepoProperties),
            GitRepoUser = proplists:get_value(user, RepoProperties),
            GitRepoGroup = proplists:get_value(group, RepoProperties),
            GitArgs = proplists:get_value(git_args, RepoProperties, []),
            GitCloneFrom = proplists:get_value(git_clone_from, RepoProperties, ssh_url),
            MonitName = proplists:get_value(monit_name, RepoProperties),
            {ok, MonitName, {GitRepoPath, GitRepoUser, GitRepoGroup,
                             GitArgs, GitCloneFrom}};
        false ->
            first_config(Ref, RepoName, RepoFullName, Rest)
    end;
first_config(Ref, RepoName, RepoFullName, [ _WrongRepo | Rest]) ->
    first_config(Ref, RepoName, RepoFullName, Rest);
first_config(_,_,_,Config) ->
    ErrMsg = io_lib:format("Invalid configuration: {apps, ~p}", [Config]),
    {error, ErrMsg}.
