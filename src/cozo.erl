%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc Main interface to CozoDB Erlang NIF `cozo_nif' module.
%%% @end
%%%===================================================================
-module(cozo).
-export([open/0, open/1, open/2, open/3]).
-export([close/1]).
-export([run/2,run/3,run/4]).
-export([import_relations/2, export_relations/2]).
-export([backup/2, restore/2, import_backup/2]).

% manage relations.
-export([list_relations/1]).
-export([remove_relation/2, remove_relations/2]).
-export([create_relation/3, replace_relation/3]).
-export([put_row/3, update_row/3, remove_row/3]).
-export([ensure_row/3, ensure_not_row/3]).

% extra management functions.
-export([list_columns/2, list_indices/2]).
-export([explain/2, describe/3]).
-export([get_triggers/2]).
-export([set_access_level/3, set_access_levels/3]).
-export([get_running_queries/1, kill/2, compact/1]).

% define cozo types and records.
-type db_id()         :: pos_integer().
-type db_engine()     :: mem | sqlite | rocksdb.
-type db_path()       :: string().
-type db_options()    :: map().
-type db_parent()     :: pid().
-record(?MODULE, { id = undefined        :: undefined | db_id()
                 , db_engine = mem       :: db_engine()
                 , db_path = ""          :: db_path()
                 , db_options = #{}      :: db_options()
                 , db_parent = undefined :: undefined | db_parent()
                 }).

%%--------------------------------------------------------------------
%% @doc open the database with mem engine, with random path and default
%%      options.
%%
%% == Examples ==
%%
%% ```
%% rr(cozo)
%% {ok, {0, State}} = cozo:open().
%% #cozo{ id = 0 
%%      , db_engine = mem
%%      , db_path = "/tmp/cozodb_Lq4yHM0RbGxbIwlyPBcNPyqPEj7O4msJ"
%%      , db_options = #{}
%%      , db_parent = <0.161>
%% } = State.
%% '''
%%
%% @see open/1
%% @see open/2
%% @see open/3
%% @end
%%--------------------------------------------------------------------
-spec open() -> Return when
      Return :: {ok, {db_id(), #?MODULE{}}} 
              | {error, term()}.

open() -> open(mem).

%%--------------------------------------------------------------------
%% @doc open a new database with custom engine and random path. The
%% random path is set to `/tmp` by default and the filename is prefixed
%% by `cozodb_'.
%%
%% == Examples ==
%%
%% ```
%% rr(cozo).
%% {ok {0, #cozo{ db_path = Path }}} = open(mem).
%% "/tmp/cozodb_aPmybeM4XTudF5qJPnG5hJusV2Evq5au" = Path.
%%
%% {ok, {1, #cozo{ db_path = SqlitePath }}} = open(sqlite).
%% "/tmp/cozodb_TDE4dQKtiXHIUjWyNEaAo1f7LAxrYk4O" = SqlitePath.
%%
%% {ok, {1, #cozo{ db_path = RocksDbPath }}} = open(rocksdb).
%% "/tmp/cozodb_S8jAUccVnge0cKfci9FYvjrK1O6fAO1S" = RocksDbPath
%% '''
%%
%% @see open/2
%% @see open/3
%% @end
%%--------------------------------------------------------------------
-spec open(Engine) -> Return when
      Engine :: db_engine(),
      Return :: {ok, {db_id(), #?MODULE{}}} 
              | {error, term()}.

open(Engine) ->
    Path = random_filepath("/tmp", "cozodb_", 32),
    open(Engine, Path).

%%--------------------------------------------------------------------
%% @doc open a database with a custom engine and path.
%%
%% == Examples ==
%%
%% ```
%% {ok, {0, State}} = cozo:open(sqlite, "/tmp/database.db").
%% '''
%%
%% @see open/3
%% @end
%%--------------------------------------------------------------------
-spec open(Engine, Path) -> Return when
      Engine :: db_engine(),
      Path :: db_path(),
      Return :: {ok, {db_id(), #?MODULE{}}} 
              | {error, term()}.

open(Engine, Path) ->
    open(Engine, Path, #{}).

%%--------------------------------------------------------------------
%% @doc open a new databse with custom path and options.
%%
%% == Examples ==
%%
%% ```
%% {ok, {0, State}} = cozo:open(rocksdb, "/tmp/rocks.db", #{}).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec open(Engine, Path, DbOptions) -> Return when
      Engine :: db_engine(),
      Path :: db_path(),
      DbOptions :: db_options(),
      Return :: {ok, {db_id(), #?MODULE{}}} 
              | {error, term()}.

open(Engine, Path, DbOptions) ->
    open1(Engine, Path, DbOptions, #?MODULE{}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc validate supported engine.
%% @end
%%--------------------------------------------------------------------
open1(mem, Path, DbOptions, State) -> 
    NewState = State#?MODULE{ db_engine = mem },
    open2("mem\n", Path, DbOptions, NewState);
open1(sqlite, Path, DbOptions, State) -> 
    NewState = State#?MODULE{ db_engine = sqlite },
    open2("sqlite\n", Path, DbOptions, NewState);
open1(rocksdb, Path, DbOptions,  State) -> 
    NewState = State#?MODULE{ db_engine = rocksdb },
    open2("rocksdb\n", Path, DbOptions, NewState);
open1(Engine, _, _, _) -> 
    {error, {bad_engine,  Engine}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc validate path.
%% @todo check if the path exist (or not).
%% @end
%%--------------------------------------------------------------------
open2(Engine, Path, DbOptions, State) 
  when is_list(Path) ->
    NewState = State#?MODULE{ db_path = Path },
    open3(Engine, Path ++ "\n", DbOptions, NewState);
open2(_, Path, _, _) ->
    {error, {bad_path, Path}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc check DbOptions and convert it to json.
%% @end
%%--------------------------------------------------------------------
open3(Engine, Path, DbOptions, State)
  when is_map(DbOptions) ->
    try 
        Json = thoas:encode(DbOptions),
        EncodedOptions = binary_to_list(Json) ++ "\n",
        NewState = State#?MODULE{ db_options = DbOptions },
        open_nif(Engine, Path, EncodedOptions, NewState)
    catch
        _Error:Reason ->
            {error, Reason}
    end;
open3(_, _, DbOptions, _) ->
    {error, {bad_options, DbOptions}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc open the nif and return db state.
%% @end
%%--------------------------------------------------------------------
open_nif(Engine, Path, DbOptions, State) ->
    case cozo_nif:open_db(Engine, Path, DbOptions) of
        {ok, DbId} ->
            NewState = State#?MODULE{ id = DbId 
                                    , db_parent = self()
                                    },
            {ok, {DbId, NewState}};
        Elsewise ->
            Elsewise
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
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_relations(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
list_relations(Db) ->
  run(Db, "::relations").

%%--------------------------------------------------------------------
%% @doc explain a query.
%%
%% == Examples ==
%%
%% ```
%% Query = "?[] <- [['hello', 'world', 'Cozo!']]",
%% {ok, _} = (Db, Query).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
explain(Db, Query) ->
  Command = string:join(["::explain", "{", Query, "}"], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc list columns
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_columns(Db, Column).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
list_columns(Db, Name) ->
  Command = string:join(["::columns", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc list indices
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_indices(Db, Indice).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
list_indices(Db, Name) ->
  Command = string:join(["::indices", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc describe a relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:describe(Db, Relation).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
describe(Db, Name, Description) ->
  Command = string:join(["::describe", Name, Description ++ "?"], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc remove a stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_relations(Db, Relation).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
remove_relation(Db, Name) ->
  Command = string:join(["::remove", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc remove a stored relations.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_relations(Db, [R1, R2, R3]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
remove_relations(Db, Names) ->
  Relations = string:join(Names, ","),
  run(Db, Relations).

%%--------------------------------------------------------------------
%% @doc Display triggers
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:get_triggers(Db, Trigger).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
get_triggers(Db, Name) ->
  Command = string:join(["::show_triggers", Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:set_access_level(Db, Level, Name).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
set_access_level(Db, Level, Name) ->
  Command = string:join(["::access_level", Level, Name], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:se_access_levelss(Db, Level, [N1, N2, N3]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
set_access_levels(Db, Level, Names) ->
  Relations = string:join(Names, ","),
  set_access_level(Db, Level, Relations).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:get_running_queries(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
get_running_queries(Db) ->
  run(Db, "::running").

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:kill(Db, Id).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
kill(Db, Id) ->
  Command = string:join(["::kill", Id], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:compact(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
compact(Db) ->
  run(Db, "::compact").

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:create_relation(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
create_relation(Db, Name, Spec) ->
  Command = string:join([":create", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:replace_relation(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
replace_relation(Db, Name, Spec) ->
  Command = string:join([":replace", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:put_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
put_row(Db, Name, Spec) ->
  Command = string:join([":put", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:update_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
update_row(Db, Name, Spec) ->
  Command = string:join([":update", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
remove_row(Db, Name, Spec) ->
  Command = string:join([":rm", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:ensure_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
ensure_row(Db, Name, Spec) ->
  Command = string:join([":ensure", Name, Spec], " "),
  run(Db, Command).

%%--------------------------------------------------------------------
%% @doc
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:ensure_not_row(Db, Name, Spec).
%% '''
%%
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

%%--------------------------------------------------------------------
%% @hidden
%% @doc generate a random filename
%% @end
%%--------------------------------------------------------------------
-spec random_filepath(Path, Prefix, Length) -> Return when
      Path :: string(),
      Prefix :: string(),
      Length :: pos_integer(),
      Return :: string().

random_filepath(Path, Prefix, Length) ->
    Alphabet = 
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "0123456789",
    AlphabetLength = length(Alphabet),
    RandomString = crypto:strong_rand_bytes(Length),
    RandomName = [ lists:nth((X rem AlphabetLength)+1, Alphabet) 
                   || <<X>> <= RandomString ],
    PrefixName = Prefix ++ RandomName,
    filename:join([Path, PrefixName]).
