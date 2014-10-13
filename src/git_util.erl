-module(git_util).

-export([pull/2]).

%% as bashodeploy user:
%% cd /home/zdsms/zendesk-sms
%% git checkout master
%% git fetch basho
%% git reset --hard FETCH_HEAD
%% git clean -fxd

pull(GitRepoPath, GitRepoUser) ->
    {ok, TmpFile} = get_tmp_file(),
    ok = build_script(TmpFile, GitRepoPath),
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

build_script(TmpFile, GitRepoPath) ->
    {ok, GitPath} = application:get_env(autodeploy, git),
    {ok, F} = file:open(TmpFile, [write]),
    ok = file:write(F, "#!/bin/sh\n"),
    ok = file:write(F, "set -o errexit\n"),
    ok = file:write(F, "whoami\n"),
    ok = io:format(F, "cd ~s~n", [GitRepoPath]),
    ok = io:format(F, "~s checkout master~n", [GitPath]),
    ok = io:format(F, "~s fetch basho~n", [GitPath]),
    ok = io:format(F, "~s reset --hard FETCH_HEAD~n", [GitPath]),
    ok = io:format(F, "~s clean -fxd~n", [GitPath]),
    ok = file:close(F).
