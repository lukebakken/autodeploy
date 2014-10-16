-module(git_util).

-export([clone/2]).

clone(GitCloneUrl, {GitRepoPath, GitRepoUser, GitRepoGroup}) ->
    {ok, TmpFile} = get_tmp_file(),
    ok = build_script(TmpFile, GitCloneUrl, GitRepoPath, GitRepoUser, GitRepoGroup),
    % TODO: add -x when debug/test
    % TODO: how best to globally indicate debug vs production
    GitCmd = ["/usr/bin/sudo", "-u", GitRepoUser, "-g", GitRepoGroup, "/bin/sh", TmpFile],
    lager:debug("git cmd: ~p", [GitCmd]),
    RunResult = os:cmd(string:join(GitCmd, " ")),
    lager:debug("git result: ~p", [RunResult]),
    ok = file:delete(TmpFile),
    {ok, RunResult}.

get_tmp_file() ->
    {ok, TmpDir} = application:get_env(autodeploy, tmp_dir),
    TmpName = erlang:phash2(make_ref()),
    TmpFile = filename:join(TmpDir, lists:concat([TmpName, ".tmp"])),
    {ok, TmpFile}.

build_script(TmpFile, GitCloneUrl, GitRepoPath, GitRepoUser, GitRepoGroup) ->
    {ok, GitPath} = application:get_env(autodeploy, git),
    {ok, F} = file:open(TmpFile, [write]),
    ok = file:write(F, "#!/bin/sh\n"),
    ok = file:write(F, "set -o errexit\n"),
    ok = file:write(F, "id -a\n"),
    ok = io:format(F, "find ~s -delete~n", [GitRepoPath]),
    ok = io:format(F, "~s clone --verbose --progress ~s ~s > /tmp/clone.out 2>&1~n", [GitPath, GitCloneUrl, GitRepoPath]),
    %% ok = io:format(F, "/bin/chown -vR ~s:~s ~s>> /tmp/clone.out 2>&1~n", [GitRepoUser, GitRepoGroup, GitRepoPath]),
    ok = file:write(F, "exit 0\n"),
    ok = file:close(F).
