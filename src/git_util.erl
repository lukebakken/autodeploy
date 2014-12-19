-module(git_util).

-export([clone/2]).

clone(GitCloneUrl, {GitRepoPath, GitRepoUser, GitRepoGroup}) ->
    clone(GitCloneUrl, {GitRepoPath, GitRepoUser, GitRepoGroup,[]});
clone(GitCloneUrl, {GitRepoPath, GitRepoUser, GitRepoGroup, GitArgs}) ->
    {ok, TmpFile} = file_util:get_tmp_file(),
    ok = build_script(TmpFile, GitCloneUrl, GitRepoPath, GitArgs),
    % TODO: how best to globally indicate debug vs production
    GitCmd = ["/usr/bin/sudo", "-u", GitRepoUser, "-g", GitRepoGroup, "/bin/sh", TmpFile],
    lager:debug("git cmd: ~p", [GitCmd]),
    RunResult = os:cmd(string:join(GitCmd, " ")),
    lager:debug("git result: ~p", [RunResult]),
    ok = file:delete(TmpFile),
    {ok, RunResult}.

build_script(TmpFile, GitCloneUrl, GitRepoPath, GitArgs) ->
    {ok, GitPath} = application:get_env(autodeploy, git),
    {ok, F} = file:open(TmpFile, [write]),
    ok = file:write(F, "#!/bin/sh\n"),
    ok = file:write(F, "id -a\n"),
    ok = io:format(F, "mkdir -p ~s~n", [GitRepoPath]),
    ok = file:write(F, "set -o errexit\n"),
    ok = io:format(F, "cd ~s~n", [GitRepoPath]),
    ok = file:write(F, "find . -delete\n"),
    ok = file:write(F, "cd ..\n"),
    ok = io:format(F, "~s clone --quiet ~s ~s ~s 2>&1~n", [GitPath, GitArgs, GitCloneUrl, GitRepoPath]),
    ok = file:write(F, "exit $?\n"),
    ok = file:close(F).

