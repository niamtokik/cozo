%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc Main interface to CozoDB Erlang NIF `cozo_nif' module.
%%% @end
%%%===================================================================
-module(cozo).
-export([open/0, open/2]).
-export([close/1]).
-export([run/2,run/3,run/4]).
-export([import_relations/2, export_relations/2]).
-export([backup/2, restore/2, import_backup/2]).

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
      cozo_nif:open_db("mem\n", Path ++ "\n");
    {sqlite, Path} ->
      cozo_nif:open_db("sqlite\n", Path ++ "\n");
    {rocksdb, Path} ->
      cozo_nif:open_db("rocksdb\n", Path ++ "\n")
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
    cozo_nif:close_db(Db).

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
-spec run(Db, Query, Params, Mutable) -> Return when
      Db :: pos_integer(),
      Query :: string(),
      Params :: map(),
      Mutable :: boolean(),
      Return :: {ok, string()} | {error, term()}.

run(Db, Query, Params, Mutable)
  when is_integer(Db) andalso is_list(Query) andalso
       is_map(Params) andalso is_boolean(Mutable) ->
    NewQuery = Query ++ "\n",
    NewParams = binary_to_list(thoas:encode(Params)) ++ "\n",
    case {NewQuery, NewParams, Mutable} of
      {"\n", "\n", _} ->
        {error, "no query and no params"};
      {NewQuery, NewParams, true} ->
        run_query_parser(Db, NewQuery, NewParams, 0);
      {NewQuery, NewParams, false} ->
        run_query_parser(Db, NewQuery, NewParams, 1)
    end.

%%--------------------------------------------------------------------
%% @doc import relations as json.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:import_relations(Db, #{}).
%% '''
%% 
%% @end
%%--------------------------------------------------------------------
-spec import_relations(Db, Json) -> Return when
      Db :: pos_integer(),
      Json :: map(),
      Return :: term().

import_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        cozo_nif:import_relations_db(Db, binary_to_list(EncodedJson) ++ "\n");
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc export database relationships from json as map.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:export_relations(Db, #{}).
%% '''
%% 
%% @end
%%--------------------------------------------------------------------
-spec export_relations(Db, Json) -> Return when
      Db :: pos_integer(),
      Json :: map(),
      Return :: term().

export_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        AsList = binary_to_list(EncodedJson) ++ "\n",
        cozo_nif:export_relations_db(Db, AsList);
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc backup a database to a file.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:backup(Db, "/tmp/backup.db").
%% '''
%% 
%% @end
%%--------------------------------------------------------------------
-spec backup(Db, OutPath) -> Return when
      Db :: pos_integer(),
      OutPath :: string(),
      Return :: term().

backup(Db, OutPath)
  when is_integer(Db) andalso is_list(OutPath) ->
    cozo_nif:backup_db(Db, OutPath ++ "\n").

%%--------------------------------------------------------------------
%% @doc restore a database based from a backup path.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:restore(Db, "/tmp/backup.db").
%% '''
%% 
%% @end
%%--------------------------------------------------------------------
-spec restore(Db, InPath) -> Return when
      Db :: pos_integer(),
      InPath :: string(),
      Return :: term().

restore(Db, InPath)
  when is_integer(Db) andalso is_list(InPath) ->
    cozo_nif:restore_db(Db, InPath ++ "\n").

%%--------------------------------------------------------------------
%% @doc import a database backup from json like object as map.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:import_backup(Db, #{}).
%% '''
%% 
%%
%% @end
%%--------------------------------------------------------------------
-spec import_backup(Db, Json) -> Return when
      Db :: pos_integer(),
      Json :: map(),
      Return :: term().

import_backup(Db, Json)
  when is_integer(Db) andalso is_map(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        cozo_nif:import_backup_db(Db, binary_to_list(EncodedJson) ++ "\n");
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc return the result in decoded json.
%% @end
%%--------------------------------------------------------------------
run_query_parser(Db, Query, Params, Mutability) ->
  case cozo_nif:run_query(Db, Query, Params, Mutability) of
    {ok, Result} -> decode_json(Result);
    Elsewise -> Elsewise
  end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc wrapper around JSON decoder.
%% @end
%%--------------------------------------------------------------------
decode_json(Message) ->
    case thoas:decode(Message) of
	{ok, Decoded} -> {ok, Decoded};
	{error, Error} -> {error, {Error, Message}};
	Elsewise -> Elsewise
    end.

	     
