%%%===================================================================
%%% @author Mathieu Kerjouan
%%%
%%% @doc Raw NIF implementation of cozodb. The library must be
%%% compiled first.
%%% @end
%%%===================================================================
-module(cozo).
-export([open/0, open/2]).
-export([close/1]).
-export([run/2,run/3,run/4]).
-export([import_relations/2, export_relations/2]).
-export([backup/2, restore/2, import_backup/2]).
-nifs([open_db/2,close_db/1,run_query/4]).
% -nifs([import_relations_db/2, export_relations_db/2]).
% -nifs([backup_db/2, restore_db/2, import_backup_db/2]).
-on_load(init/0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init() ->
  ok = erlang:load_nif("./c_src/cozo_nif", 0).

%%--------------------------------------------------------------------
%% @doc open the database with mem engine.
%%
%% == Examples ==
%%
%% ```
%% {ok, 0} = cozo:open().
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec open() -> Return when
      Return :: {ok, Db} | {error, open_error},
      Db :: pos_integer().

open() -> open(mem, "/tmp/cozo_mem.db").

%%--------------------------------------------------------------------
%% @doc open a database with a custom engine and path.
%%
%% == Examples ==
%%
%% ```
%% {ok, 0} = cozo:open(mem, "/tmp/database.db").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec open(Engine, Path) -> Return when
      Engine :: mem | sqlite | rocksdb,
      Path :: string(),
      Return :: {ok, Db} | {error, open_error},
      Db :: pos_integer().
      
open(Engine, Path) ->
  case {Engine, Path} of
    {mem, Path} ->
      open_db("mem\n", Path ++ "\n");
    {sqlite, Path} ->
      open_db("sqlite\n", Path ++ "\n");
    {rocksdb, Path} ->
      open_db("rocksdb\n", Path ++ "\n")
  end.

%%--------------------------------------------------------------------
%% @doc close an opened database.
%%
%% == Examples ==
%%
%% ```
%% ok = cozo:close(0).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec close(Db) -> Return when
      Db :: pos_integer(),
      Return :: ok | {error, close_error}.
      
close(Db)
  when is_integer(Db) ->
    close_db(Db).

%%--------------------------------------------------------------------
%% @doc run a query on defined db and custom query. No parameters are
%% passed and immutability is set to true.
%%
%% == Examples ==
%%
%% ```
%% {ok, #{ <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>]
%%         <<"next">> => null,
%%         <<"ok">> => true,
%%         <<"rows">> => [[1,2,3]],
%%         <<"took">> => 2.40886e-4}
%% } = cozo:run(0, "?[] <- [[1, 2, 3]]").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Db, Query) -> Return when
      Db :: pos_integer(),
      Query :: string(),
      Return :: {ok, string()} | {error, term()}.

run(Db, Query) ->
  run(Db, Query, #{}, true).

%%--------------------------------------------------------------------
%% @doc run a query on defined db with custom params. Immutability is
%% set to true.
%%
%% == Examples ==
%%
%% ```
%% {ok, #{ <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>]
%%         <<"next">> => null,
%%         <<"ok">> => true,
%%         <<"rows">> => [[1,2,3]],
%%         <<"took">> => 2.40886e-4}
%% } = cozo:run(0, "?[] <- [[1, 2, 3]]", #{}).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Db, Query, Params) -> Return when
      Db :: pos_integer(),
      Query :: string(),
      Params :: map(),
      Return :: {ok, string()} | {error, term()}.

run(Db, Query, Params) ->
  run(Db, Query, Params, true).

%%--------------------------------------------------------------------
%% @doc run a query.
%%
%% == Examples ==
%%
%% ```
%% {ok, #{ <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>]
%%         <<"next">> => null,
%%         <<"ok">> => true,
%%         <<"rows">> => [[1,2,3]],
%%         <<"took">> => 2.40886e-4}
%% } = cozo:run(0, "?[] <- [[1, 2, 3]]", #{ limit => 10 }, falsex).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Db, Query, Params, Immutable) -> Return when
      Db :: pos_integer(),
      Query :: string(),
      Params :: map(),
      Immutable :: boolean(),
      Return :: {ok, string()} | {error, term()}.

run(Db, Query, Params, Immutable)
  when is_integer(Db) andalso is_list(Query) andalso
       is_map(Params) andalso is_boolean(Immutable) ->
    NewQuery = Query ++ "\n",
    NewParams = binary_to_list(thoas:encode(Params)) ++ "\n",
    case {NewQuery, NewParams, Immutable} of
      {"\n", "\n", _} ->
        {error, "no query and no params"};
      {NewQuery, NewParams, true} ->
        run_parser(Db, NewQuery, NewParams, 1);
      {NewQuery, NewParams, false} ->
        run_parser(Db, NewQuery, NewParams, 0)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
import_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        import_relations_db(Db, binary_to_list(EncodedJson) ++ "\n");
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
export_relations(Db, Json) -> error.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
backup(Db, OutPath) -> error.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
restore(Db, InPath) -> error.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
import_backup(Db, Json) -> error.

%%--------------------------------------------------------------------
%% @hidden
%% @doc return the result in decoded json.
%% @end
%%--------------------------------------------------------------------
run_parser(Db, Query, Params, Mutability) ->
  case run_query(Db, Query, Params, Mutability) of
    {ok, Result} -> thoas:decode(Result);
    Elsewise -> Elsewise
  end.

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

