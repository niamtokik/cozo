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

% manage relations
-export([list_relations/1]).
-export([remove_relation/2, remove_relations/2]).
-export([create_relation/3, replace_relation/3]).
-export([put_row/3, update_row/3, remove_row/3]).
-export([ensure_row/3, ensure_not_row/3]).

% extra management functions
-export([list_columns/2, list_indices/2]).
-export([explain/2, describe/3]).
-export([show_triggers/2]).
-export([set_access_level/3, set_access_levels/3]).
-export([get_running_queries/1, kill/2, compact/1]).

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
%% see https://docs.cozodb.org/en/latest/nonscript.html#API.import_relations
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
      Json :: map() | list(),
      Return :: term().

import_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) orelse is_list(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        cozo_nif:import_relations_db(Db, binary_to_list(EncodedJson) ++ "\n");
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc export database relationships from json as map.
%%
%% see https://docs.cozodb.org/en/latest/nonscript.html#API.export_relations
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
      Json :: map() | list(),
      Return :: term().

export_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) orelse is_list(Json) ->
    case thoas:encode(Json) of
      {ok, EncodedJson} ->
        AsList = binary_to_list(EncodedJson) ++ "\n",
        cozo_nif:export_relations_db(Db, AsList);
      Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc backup a database to a file.
%%
%% see https://docs.cozodb.org/en/latest/nonscript.html#API.backup
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
%% see https://docs.cozodb.org/en/latest/nonscript.html#API.restore
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
%% see https://docs.cozodb.org/en/latest/nonscript.html#API.import_from_backup
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
%% @doc returns the list of relations
%% @end
%%--------------------------------------------------------------------
list_relations(Db) ->
  run(Db, "::relations").

%%--------------------------------------------------------------------
%% @doc explain a query.
%% @end
%%--------------------------------------------------------------------
explain(Db, Query) ->
  Command = string:join(["::explain", "{", Query, "}"], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc list columns
%% @end
%%--------------------------------------------------------------------
list_columns(Db, Name) ->
  Command = string:join(["::columns", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc list indices
%% @end
%%--------------------------------------------------------------------
list_indices(Db, Name) ->
  Command = string:join(["::indices", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc describe a relation.
%% @end
%%--------------------------------------------------------------------
describe(Db, Name, Description) ->
  Command = string:join(["::describe", Name, Description ++ "?"], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc remove a stored relation.
%% @end
%%--------------------------------------------------------------------
remove_relation(Db, Name) ->
  Command = string:join(["::remove", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc remove a stored relations.
%% @end
%%--------------------------------------------------------------------
remove_relations(Db, Names) ->
  Relations = string:join(Names, ","),
  run(Db, Relations).

%%--------------------------------------------------------------------
%% @doc Display triggers
%% @end
%%--------------------------------------------------------------------
show_triggers(Db, Name) ->
  Command = string:join(["::show_triggers", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
set_access_level(Db, Level, Name) ->
  Command = string:join(["::access_level", Level, Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
set_access_levels(Db, Level, Names) ->
  Relations = string:join(Names, ","),
  set_access_level(Db, Level, Relations).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_running_queries(Db) ->
  run(Db, "::running").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
kill(Db, Id) ->
  Command = string:join(["::kill", Id], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
compact(Db) ->
  run(Db, "::compact").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
create_relation(Db, Name, Spec) ->
  Command = string:join([":create", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
replace_relation(Db, Name, Spec) ->
  Command = string:join([":replace", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
put_row(Db, Name, Spec) ->
  Command = string:join([":put", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
update_row(Db, Name, Spec) ->
  Command = string:join([":update", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
remove_row(Db, Name, Spec) ->
  Command = string:join([":rm", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
ensure_row(Db, Name, Spec) ->
  Command = string:join([":ensure", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
ensure_not_row(Db, Name, Spec) ->
  Command = string:join([":ensure_not", Name, Spec], " "),
  run(Db, Command).

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

	     
