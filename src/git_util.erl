-module(git_util).

-export([clone/3]).

%% as bashodeploy user:
%% cd /home/zdsms/zendesk-sms
%% git checkout master
%% git fetch basho
%% git reset --hard FETCH_HEAD
%% git clean -fxd

clone(GitCloneUrl, GitRepoPath, GitRepoUser) ->
    {ok, TmpFile} = get_tmp_file(),
    ok = build_script(TmpFile, GitCloneUrl, GitRepoPath),
    % TODO: add -x when debug/test
    % TODO: how best to globally indicate debug vs production
    {ok, RunResult} = exec:run(["/bin/sh", TmpFile], [sync, stdout, {user, GitRepoUser}]),
    lager:debug("git result: ~p", [RunResult]),
    ok = file:delete(TmpFile),
    {ok, RunResult}.

get_tmp_file() ->
    {ok, TmpDir} = application:get_env(autodeploy, tmp_dir),
    TmpName = erlang:phash2(make_ref()),
    TmpFile = filename:join(TmpDir, lists:concat([TmpName, ".tmp"])),
    {ok, TmpFile}.

build_script(TmpFile, GitCloneUrl, GitRepoPath) ->
    {ok, GitPath} = application:get_env(autodeploy, git),
    {ok, F} = file:open(TmpFile, [write]),
    ok = file:write(F, "#!/bin/sh\n"),
    ok = file:write(F, "set -o errexit\n"),
    ok = file:write(F, "whoami\n"),
    ok = io:format(F, "rm -rf ~s~n", [GitRepoPath]),
    ok = io:format(F, "~s clone --quiet ~s ~s~n", [GitPath, GitCloneUrl, GitRepoPath]),
    ok = file:write(F, "exit 0\n"),
    ok = file:close(F).
