-module(git_util).

-export([clone/2]).

clone(RepoCloneList, {GitRepoPath, GitRepoUser, GitRepoGroup}) ->
    clone(RepoCloneList, {GitRepoPath, GitRepoUser, GitRepoGroup, [], ssh_url});
clone(RepoCloneList, {GitRepoPath, GitRepoUser, GitRepoGroup, GitArgs, GitCloneFrom}) ->
    GitCloneUrl = proplists:get_value(GitCloneFrom, RepoCloneList),
    ok = lager:debug("git clone url: ~p", [GitCloneUrl]),
    {ok, TmpFile} = file_util:get_tmp_file(),
    ok = build_script(TmpFile, GitCloneUrl, GitRepoPath, GitArgs),
    % TODO: how best to globally indicate debug vs production
    GitCmd = ["/usr/bin/sudo", "-u", GitRepoUser, "-g", GitRepoGroup, "/bin/sh", TmpFile],
    ok = lager:debug("git cmd: ~p", [GitCmd]),
    RunResult = os:cmd(string:join(GitCmd, " ")),
    ok = lager:debug("git output: ~s", [RunResult]),
    misc_util:file_delete(TmpFile),
    {ok, RunResult}.

build_script(TmpFile, GitCloneUrl, GitRepoPath, GitArgs) ->
    {ok, GitPath} = application:get_env(autodeploy, git),
    {ok, F} = file:open(TmpFile, [write]),
    ok = file:write(F, "#!/bin/sh\n"),
    ok = file:write(F, "id -a\n"),
    ok = io:format(F, "mkdir -p ~s~n", [GitRepoPath]),
    ok = file:write(F, "set -o errexit\n"),
    ok = io:format(F, "cd ~s~n", [GitRepoPath]),
    ok = file:write(F, "[ -f ./autodeploy.sh ] && [ -x ./autodeploy.sh ] && ./autodeploy.sh pre\n"),
    ok = file:write(F, "[ -f ./autodeploy.sh ] && [ ! -x ./autodeploy.sh ] && /bin/sh ./autodeploy.sh pre\n"),
    ok = file:write(F, "find . -delete\n"),
    ok = file:write(F, "cd ..\n"),
    ok = io:format(F, "~s clone --quiet ~s ~s ~s 2>&1~n", [GitPath, GitArgs, GitCloneUrl, GitRepoPath]),
    ok = io:format(F, "cd ~s~n", [GitRepoPath]),
    ok = file:write(F, "[ -f ./autodeploy.sh ] && [ -x ./autodeploy.sh ] && ./autodeploy.sh post\n"),
    ok = file:write(F, "[ -f ./autodeploy.sh ] && [ ! -x ./autodeploy.sh ] && /bin/sh ./autodeploy.sh post\n"),
    ok = file:close(F).

