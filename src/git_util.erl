-module(git_util).

-export([clone/2]).

clone(GitCloneUrl, {GitRepoPath, GitRepoUser, GitRepoGroup}) ->
    {ok, TmpFile} = get_tmp_file(),
    ok = build_script(TmpFile, GitCloneUrl, GitRepoPath),
    % TODO: add -x when debug/test
    % TODO: how best to globally indicate debug vs production
    % NB: must use "sudo" as erlexec's run as user/group is broken
    {ok, RunResult} = exec:run(["/usr/bin/sudo", "-u", GitRepoUser, "-g", GitRepoGroup, "/bin/sh", TmpFile], [sync, stdout]),
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
    ok = file:write(F, "id -a\n"),
    ok = io:format(F, "echo 'rm -rf ~s > /tmp/clone.out 2>&1'~n", [GitRepoPath]),
    ok = io:format(F, "rm -rf ~s > /tmp/clone.out 2>&1~n", [GitRepoPath]),
    ok = io:format(F, "echo '~s clone --verbose --progress ~s ~s > /tmp/clone.out 2>&1'~n", [GitPath, GitCloneUrl, GitRepoPath]),
    ok = io:format(F, "~s clone --verbose --progress ~s ~s >> /tmp/clone.out 2>&1~n", [GitPath, GitCloneUrl, GitRepoPath]),
    ok = file:write(F, "exit 0\n"),
    ok = file:close(F).
