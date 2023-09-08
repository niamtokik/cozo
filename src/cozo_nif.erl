%%%===================================================================
%%% @author Mathieu Kerjouan
%%%
%%% @doc Raw NIF implementation of cozodb. The library must be
%%% compiled first.
%%% @end
%%%===================================================================
-module(cozo_nif).
-export([open_db/2, close_db/1, run_query/4
        ,import_relations_db/2, export_relations_db/2
        ,backup_db/2, restore_db/2, import_backup_db/2
        ]).
-nifs([open_db/2, close_db/1, run_query/4
      ,import_relations_db/2, export_relations_db/2
      ,backup_db/2, restore_db/2, import_backup_db/2
      ]).
-on_load(init/0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init() -> init("cozo_nif").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init(Path) ->
  Priv = priv_dir(),
  Lib = filename:join(Priv, Path),
  ok = erlang:load_nif(Lib, 0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc open_db nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L35
%% @end
%%--------------------------------------------------------------------
open_db(_Engine, _Path) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc run_query nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L62
%% @end
%%--------------------------------------------------------------------
run_query(_Id, _Script, _Params, _Immutable) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc close_db nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L45
%% @end
%%--------------------------------------------------------------------
close_db(_Id) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
import_relations_db(_Id, _Json) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
export_relations_db(_Id, _Json) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
backup_db(_Id, _Path) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
restore_db(_Id, _Path) ->
  exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
import_backup_db(_Id, _Path) ->
  exit(nif_library_not_loaded).





%% =============================================================================
%% PRiVATE
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Returns the app's priv dir
%% @end
%% -----------------------------------------------------------------------------
priv_dir() ->
    case code:priv_dir(cozo) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                FN when is_list(FN) ->
                  filename:join([filename:dirname(FN), "..", "priv"]);
                _ ->
                  "../priv"
            end;
        Val ->
            Val
    end.
