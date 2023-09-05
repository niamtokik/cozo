-module(cozo).
-export([open/0,close/1,run/1]).
-nifs([open/0,close/1,run/1]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./c_src/cozo_nif", 0).

open() ->
  exit(nif_library_not_loaded).

run(Id) ->
  exit(nif_library_not_loaded).

close(Id) ->
  exit(nif_library_not_loaded).