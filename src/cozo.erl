%%%===================================================================
%%% Copyright (c) 2023 Mathieu Kerjouan
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above
%%% copyright notice, this list of conditions and the following
%%% disclaimer in the documentation and/or other materials provided
%%% with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%%% @copyright 2023 Mathieu Kerjouan
%%% @author Mathieu Kerjouan
%%%
%%% @doc Main interface to CozoDB Erlang NIF `cozo_nif' module.
%%% @end
%%%===================================================================
-module(cozo).

% API.
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

% helpers
-export([create_filepath/3]).
-export([json_decoder/0, json_encoder/0]).

% includes.
-include_lib("kernel/include/logger.hrl").
-include("cozo.hrl").

%%--------------------------------------------------------------------
%% @doc returns the default json encoder (thoas)
%% @end
%%--------------------------------------------------------------------
-spec json_encoder() -> atom().
json_encoder() -> application:get_env(cozo, json_parser, thoas).

%%--------------------------------------------------------------------
%% @doc returns the default json decoder (thoas)
%% @end
%%--------------------------------------------------------------------
-spec json_decoder() -> atom().
json_decoder() -> application:get_env(cozo, json_parser, thoas).

%%--------------------------------------------------------------------
%% @doc Open the database with mem engine, with random path and
%%      default options.
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
      Return :: {ok, {db_id(), #cozo{}}}
	      | {error, term()}.

open() ->
    DefaultEngine = application:get_env(cozo, engine, mem),
    open(DefaultEngine).

%%--------------------------------------------------------------------
%% @doc Open a new database with custom engine and random path. The
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
      Return :: {ok, {db_id(), #cozo{}}}
	      | {error, term()}.

open(Engine) ->
    DefaultPath = application:get_env(cozo, db_path, "/tmp"),
    DefaultPrefix = application:get_env(cozo, db_filename_prefix, "cozodb_"),
    DefaultLength = application:get_env(cozo, db_filename_random_length, 32),
    Path = create_filepath(DefaultPath, DefaultPrefix, DefaultLength),
    open(Engine, Path).

%%--------------------------------------------------------------------
%% @doc Open a database with a custom engine and path.
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
      Path   :: db_path(),
      Return :: {ok, {db_id(), #cozo{}}}
	      | {error, term()}.

open(Engine, Path) ->
    Options = case Engine of
		  sqlite ->
		      application:get_env(cozo, sqlite_options,  #{});
		  rocksdb ->
		      application:get_env(cozo, rocksdb_options,  #{});
		  _ -> #{}
	      end,
    open(Engine, Path, Options).

%%--------------------------------------------------------------------
%% @doc Open a new databse with custom path and options.
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
      Engine    :: db_engine(),
      Path      :: db_path(),
      DbOptions :: db_options(),
      Return    :: {ok, {db_id(), #cozo{}}}
                 | {error, term()}.

open(Engine, Path, DbOptions) ->
    open1(Engine, Path, DbOptions, #cozo{}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc validate supported engine.
%% @end
%%--------------------------------------------------------------------
open1(mem, Path, DbOptions, State) ->
    NewState = State#cozo{ db_engine = mem },
    open2("mem\n", Path, DbOptions, NewState);
open1(sqlite, Path, DbOptions, State) ->
    NewState = State#cozo{ db_engine = sqlite },
    open2("sqlite\n", Path, DbOptions, NewState);
open1(rocksdb, Path, DbOptions,  State) ->
    NewState = State#cozo{ db_engine = rocksdb },
    open2("rocksdb\n", Path, DbOptions, NewState);
open1(Engine, _, _, _) ->
    Reason = {bad_engine,  Engine},
    ?LOG_ERROR("~p~n", [{cozo, open1, Reason}]),
    {error, Reason}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc validate path.
%% @todo check if the path exist (or not).
%% @end
%%--------------------------------------------------------------------
open2(Engine, Path, DbOptions, State)
  when is_list(Path) ->
    DirName = filename:dirname(Path),
    case filelib:ensure_dir(DirName) of
	ok ->
	    NewState = State#cozo{ db_path = Path },
	    open3(Engine, Path ++ "\n", DbOptions, NewState);
	Elsewise ->
	    Elsewise
    end;
open2(_, Path, _, _) ->
    Reason = {bad_path, Path},
    ?LOG_ERROR("~p~n", [{cozo, open2, Reason}]),
    {error, Reason}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc Check DbOptions and convert it to json.
%% @end
%%--------------------------------------------------------------------
open3(Engine, Path, DbOptions, State)
  when is_map(DbOptions) ->
    try
	Encoder = json_encoder(),
	Json = Encoder:encode(DbOptions),
	EncodedOptions = binary_to_list(Json) ++ "\n",
	NewState = State#cozo{ db_options = DbOptions },
	open_nif(Engine, Path, EncodedOptions, NewState)
    catch
	_Error:Reason ->
	    {error, Reason}
    end;
open3(_, _, DbOptions, _) ->
    Reason = {bad_options, DbOptions},
    ?LOG_ERROR("~p~n", [{cozo, open3, Reason}]),
    {error, Reason}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc Open the nif and return db state.
%% @end
%%--------------------------------------------------------------------
open_nif(Engine, Path, DbOptions, State) ->
    case cozo_nif:open_db(Engine, Path, DbOptions) of
	{ok, DbId} ->
	    NewState = State#cozo{ id = DbId
				    , db_parent = self()
				    },
	    ?LOG_DEBUG("~p", [{cozo, open_nif, NewState}]),
	    {ok, {DbId, NewState}};
	Elsewise ->
	    ?LOG_ERROR("~p", [{cozo, open_nif, Elsewise}]),
	    Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Close an opened database.
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
      Db     :: db_id(),
      Return :: ok | {error, close_error}.

close(Db)
  when is_integer(Db) ->
    ?LOG_DEBUG("~p", [{cozo, close, [Db]}]),
    case cozo_nif:close_db(Db) of
	ok -> ok;
	{error, Error} -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc run a query using cozoscript on defined db and custom
%% query. No parameters are passed and immutability is set to true.
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
      Db     :: db_id(),
      Query  :: db_query(),
      Return :: query_return().

run(Db, Query) ->
    run(Db, Query, #{}, true).

%%--------------------------------------------------------------------
%% @doc run a query using cozoscript on defined db with custom
%% params. Immutability is set to true.
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
      Db     :: db_id(),
      Query  :: db_query(),
      Params :: query_params(),
      Return :: query_return().

run(Db, Query, Params) ->
    run(Db, Query, Params, true).

%%--------------------------------------------------------------------
%% @doc Run a query using cozoscript.
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
      Db      :: db_id(),
      Query   :: db_query(),
      Params  :: query_params(),
      Mutable :: query_mutable(),
      Return  :: query_return().

run(Db, Query, Params, Mutable)
  when is_binary(Query) ->
    run(Db, binary_to_list(Query), Params, Mutable);
run(_Db, "", _Params, _Mutable) ->
    {error, empty_query};
run(Db, [X|_] = Query, Params, Mutable)
  when is_list(X) ->
    NewQuery = string:join(Query, "\n"),
    run(Db, NewQuery, Params, Mutable);
run(Db, Query, Params, Mutable)
  when is_integer(Db) andalso is_list(Query) andalso
       is_map(Params) andalso is_boolean(Mutable) ->
    run1(Db, Query, Params, Mutable);
run(_Db, _Query, _Params, _Mutable) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
run1(Db, Query, Params, Mutable) ->
    NewQuery = Query ++ "\n",
    Encoder = json_encoder(),
    NewParams = binary_to_list(Encoder:encode(Params)) ++ "\n",
    case {NewQuery, NewParams, Mutable} of
	{"\n", "\n", _} ->
	    {error, "no query and no params"};
	{NewQuery, NewParams, true} ->
	    run_query_parser(Db, NewQuery, NewParams, 0);
	{NewQuery, NewParams, false} ->
	    run_query_parser(Db, NewQuery, NewParams, 1)
    end.

%%--------------------------------------------------------------------
%% @doc Import relations as json.
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
      Db     :: pos_integer(),
      Json   :: map() | list(),
      Return :: {ok, map()} 
	      | {error, term()}.

import_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) orelse is_list(Json) ->
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
	{ok, EncodedJson} ->
	    Payload =  binary_to_list(EncodedJson) ++ "\n",
	    import_relations1(Db, Payload);
	Elsewise -> Elsewise
    end.

import_relations1(Db, Json) ->
    case cozo_nif:import_relations_db(Db, Json) of
	{ok, Result} ->
	    decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Export database relationships from json as map.
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
      Db     :: pos_integer(),
      Json   :: map() | list(),
      Return :: {ok, map()}
	      | {error, term()}.

export_relations(Db, Json)
  when is_integer(Db) andalso is_map(Json) orelse is_list(Json) ->
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
	{ok, EncodedJson} ->
	    AsList = binary_to_list(EncodedJson) ++ "\n",
	    export_relations1(Db, AsList);
	Elsewise -> Elsewise
    end.

export_relations1(Db, Json) ->
    case cozo_nif:export_relations_db(Db, Json) of
	{ok, Result} ->
	    decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Backup a database to a file.
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
      Db      :: pos_integer(),
      OutPath :: string(),
      Return  :: {ok, map()}
	       | {error, term()}.

backup(Db, OutPath)
  when is_integer(Db) andalso is_list(OutPath) ->
    case cozo_nif:backup_db(Db, OutPath ++ "\n") of
	{ok, Result} ->
	    decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Restore a database based from a backup path.
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
      Db     :: pos_integer(),
      InPath :: string(),
      Return :: {ok, map()}
	      | {error, term()}.

restore(Db, InPath)
  when is_integer(Db) andalso is_list(InPath) ->
    case cozo_nif:restore_db(Db, InPath ++ "\n") of
	{ok, Result} ->
	    decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Import a database backup from json like object as map.
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
      Db     :: pos_integer(),
      Json   :: map(),
      Return :: {ok, map()}
	      | {error, term()}.

import_backup(Db, Json)
  when is_integer(Db) andalso is_map(Json) ->
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
	{ok, EncodedJson} ->
	    Payload = binary_to_list(EncodedJson) ++ "\n",
	    import_backup1(Db, Payload);
	Elsewise -> Elsewise
    end.

import_backup1(Db, Json) ->
    case cozo_nif:import_backup_db(Db, Json) of
	{ok, Result} ->
	    decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc Unstable interface. Returns the list of relations.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_relations(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec list_relations(Db) -> Return when
      Db     :: db_id(),
      Return :: query_return().

list_relations(Db) ->
    run(Db, "::relations").

%%--------------------------------------------------------------------
%% @doc Unstable interface. Explain a query.
%%
%% see https://docs.cozodb.org/en/latest/sysops.html#explain
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
-spec explain(Db, Query) -> Return when
      Db     :: db_id(),
      Query  :: string(),
      Return :: query_return().

explain(Db, Query) ->
    Command = string:join(["::explain", "{", Query, "}"], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. List all columns for the stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_columns(Db, Column).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec list_columns(Db, Name) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Return :: query_return().

list_columns(Db, Name) ->
    Command = string:join(["::columns", Name], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. List all indices for the stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:list_indices(Db, Indice).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec list_indices(Db, Name) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Return :: query_return().

list_indices(Db, Name) ->
    Command = string:join(["::indices", Name], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Describe the stored relation and store it
%% in the metadata.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:describe(Db, Relation).
%% '''
%%
%% @see run/4
%% @end
%%--------------------------------------------------------------------
-spec describe(Db, Name, Description) -> Return when
      Db          :: db_id(),
      Name        :: string(),
      Description :: string(),
      Return      :: query_return().

describe(Db, Name, Description) ->
    Command = string:join(["::describe", Name, Description ++ "?"], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Remove a stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_relations(Db, Relation).
%% '''
%%
%% @see run/4
%% @end
%%--------------------------------------------------------------------
-spec remove_relation(Db, Name) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Return :: query_return().

remove_relation(Db, Name) ->
    Command = string:join(["::remove", Name], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Remove a stored relations.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_relations(Db, [R1, R2, R3]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_relations(Db, Names) -> Return when
      Db     :: db_id(),
      Names  :: [string(), ...],
      Return :: query_return().

remove_relations(Db, Names) ->
    Relations = string:join(Names, ","),
    run(Db, Relations).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Display triggers.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:get_triggers(Db, Trigger).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec get_triggers(Db, Name) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Return :: query_return().

get_triggers(Db, Name) ->
    Command = string:join(["::show_triggers", Name], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:set_access_level(Db, Level, Name).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec set_access_level(Db, Level, Name) -> Return when
      Db     :: db_id(),
      Level  :: string(),
      Name   :: string(),
      Return :: query_return().

set_access_level(Db, Level, Name) ->
    Command = string:join(["::access_level", Level, Name], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:se_access_levelss(Db, Level, [N1, N2, N3]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec set_access_levels(Db, Level, Names) -> Return when
      Db     :: db_id(),
      Level  :: string(),
      Names  :: [string(), ...],
      Return :: query_return().

set_access_levels(Db, Level, Names) ->
    Relations = string:join(Names, ","),
    set_access_level(Db, Level, Relations).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Display running queries and their id.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:get_running_queries(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec get_running_queries(Db) -> Return when
      Db     :: db_id(),
      Return :: query_return().

get_running_queries(Db) ->
    run(Db, "::running").

%%--------------------------------------------------------------------
%% @doc Unstable interface. Kill a running query specified by id.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:kill(Db, Id).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec kill(Db, Id) -> Return when
      Db     :: db_id(),
      Id     :: string(),
      Return :: query_return().

kill(Db, Id) ->
    Command = string:join(["::kill", Id], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Instructs Cozo to run a compaction job.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:compact(Db).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec compact(Db) -> Return when
      Db     :: db_id(),
      Return :: query_return().

compact(Db) ->
    run(Db, "::compact").

%%--------------------------------------------------------------------
%% @doc Unstable interface. Create a stored relation with the given
%% name and spec. No stored relation with the same name can exist
%% beforehand.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:create_relation(Db, "stored1", "c1").
%% % :create stored1 {c1}
%%
%% {ok, _} = cozo:create_relation(Db, stored2, c1).
%% % :create stored2 {c1}
%%
%% {ok, _} = cozo:create_relation(Db, stored3, ["c1","c2","c3"]).
%% % :create stored3 {c1,c2,c3}
%%
%% {ok, _} = cozo:create_relation(Db, stored4, [c1,c2,c3]).
%% % :create stored4 {c1,c2,c3}
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec create_relation(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string() | atom(),
      Spec   :: string() | atom()
	      | [string(), ...]
	      | [atom(), ...],
      Return :: query_return().

create_relation(Db, Name, Spec) 
  when is_atom(Name) ->
    create_relation(Db, atom_to_list(Name), Spec);
create_relation(Db, Name, Spec)
  when is_atom(Spec) ->
    create_relation(Db, Name, atom_to_list(Spec));
create_relation(Db, Name, [X|_] = Specs)
  when is_atom(X) orelse is_list(X) ->
    FullSpec = [ to_list(Item) || Item <- Specs ],
    Relations = string:join(FullSpec, ","),
    create_relation(Db, Name, Relations);
create_relation(Db, Name, Spec) ->
    RawCommand = [":create", Name, "{", Spec, "}"],
    Command = string:join(RawCommand, " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @hidden
%% @doc helper to convert string or atom to string
%% @end
%%--------------------------------------------------------------------
-spec to_list(string() | atom()) -> string().

to_list(List) when is_list(List) -> List;
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Similar to `:create', except that if the
%% named stored relation exists beforehand, it is completely replaced.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:replace_relation(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec replace_relation(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

replace_relation(Db, Name, Spec) ->
    RawCommand = [":replace", Name, "{", Spec, "}"],
    Command = string:join(RawCommand, " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Put rows from the resulting relation into
%% the named stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:put_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec put_row(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

put_row(Db, Name, Spec) ->
    Command = string:join([":put", Name, Spec], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Update rows in the named stored relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:update_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec update_row(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

update_row(Db, Name, Spec) ->
    Command = string:join([":update", Name, Spec], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Remove rows from the named stored
%% relation.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:remove_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_row(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

remove_row(Db, Name, Spec) ->
    Command = string:join([":rm", Name, Spec], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Ensure that rows specified by the output
%% relation and spec exist in the database, and that no other process
%% has written to these rows when the enclosing transaction
%% commits. Useful for ensuring read-write consistency.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:ensure_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_row(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

ensure_row(Db, Name, Spec) ->
    Command = string:join([":ensure", Name, Spec], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc Unstable interface. Ensure that rows specified by the output
%% relation and spec do not exist in the database and that no other
%% process has written to these rows when the enclosing transaction
%% commits.
%%
%% == Examples ==
%%
%% ```
%% {ok, _} = cozo:ensure_not_row(Db, Name, Spec).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_not_row(Db, Name, Spec) -> Return when
      Db     :: db_id(),
      Name   :: string(),
      Spec   :: string(),
      Return :: query_return().

ensure_not_row(Db, Name, Spec) ->
    Command = string:join([":ensure_not", Name, Spec], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @hidden
%% @doc return the result in decoded json.
%% @end
%%--------------------------------------------------------------------
-spec run_query_parser(Db, Query, Params, Mutability) -> Return when
      Db         :: db_id(),
      Query      :: string(),
      Params     :: string(),
      Mutability :: 0 | 1,
      Return     :: {ok, map()}
		  | {error, any()}.

run_query_parser(Db, Query, Params, Mutability) ->
    case cozo_nif:run_query(Db, Query, Params, Mutability) of
	{ok, Result} -> decode_json(Result);
	Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc wrapper around JSON decoder, by default using `thoas'.
%% @end
%%--------------------------------------------------------------------
-spec decode_json(Message) -> Return when
      Message :: binary() | bitstring(),
      Return  :: {ok, map()}
	       | {error, term()}.

decode_json(Message) ->
    Decoder = json_decoder(),
    try Decoder:decode(Message) of
	{ok, Decoded} -> {ok, Decoded};
	{error, Error} -> {error, {Error, Message}};
	Elsewise -> Elsewise
    catch
	error:_Reason -> {error, Message}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc generate a random filename
%%
%% == Examples ==
%%
%% ```
%% "/tmp/prefix_PFin0sRLFiHPt9LU46w1Ei00bvp3b1hv"
%%   = cozo:create_filepath("/tmp", "prefix_", 32).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec create_filepath(Path, Prefix, Length) -> Return when
      Path   :: string(),
      Prefix :: string(),
      Length :: pos_integer(),
      Return :: string().

create_filepath(Path, Prefix, Length) ->
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
