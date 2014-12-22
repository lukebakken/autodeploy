-module(misc_util).
-export([file_delete/1]).

-ifdef(DEBUG).
file_delete(_File) -> ok.
-else.
file_delete(File) ->
    ok = file:delete(TmpFile).
-endif.

