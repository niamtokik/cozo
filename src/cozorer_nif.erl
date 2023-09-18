-module(cozorer_nif).
-nif([open/3, close/1]).
-export([open/3, close/1]).
-on_load(init/0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.

init() -> init("cozorer_nif").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Path) -> Return when
      Path   :: db_path(),
      Return :: ok.

init(Path) ->
    PrivDir = application:get_env(cozo, lib_path, priv_dir()),
    case application:get_env(cozo, debug, false) of
        true -> logger:set_module_level(?MODULE, debug);
        _ -> ok
    end,
    Lib = filename:join(PrivDir, Path),
    ?LOG_DEBUG("~p", [debug_message(init, [Path])]),
    ok = erlang:load_nif(Lib, 0).

