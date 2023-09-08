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
%% @doc open the database with mem engine, with random path and
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
      Return :: {ok, {db_id(), #cozo{}}}
	      | {error, term()}.

open(Engine) ->
    DefaultPath = application:get_env(cozo, db_path, "/tmp"),
    DefaultPrefix = application:get_env(cozo, db_filename_prefix, "cozodb_"),
    DefaultLength = application:get_env(cozo, db_filename_random_length, 32),
    Path = create_filepath(DefaultPath, DefaultPrefix, DefaultLength),
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
      Return :: {ok, {db_id(), #cozo{}}}
	      | {error, term()}.

open(Engine, Path) ->
    Options = application:get_env(cozo, {Engine, options},  #{}),
    open(Engine, Path, Options).

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
      Return :: {ok, {db_id(), #cozo{}}}
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
%% @doc check DbOptions and convert it to json.
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
%% @doc open the nif and return db state.
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
    ?LOG_DEBUG("~p", [{cozo, close, [Db]}]),
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
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
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
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
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
    Encoder = json_encoder(),
    case Encoder:encode(Json) of
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
explain(Db, Query) ->
    Command = string:join(["::explain", "{", Query, "}"], " "),
    run(Db, Command).

%%--------------------------------------------------------------------
%% @doc List all columns for the stored relation.
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
%% @doc List all indices for the stored relation.
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
%% @doc Describe the stored relation and store it in the metadata.
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
%% @doc Display running queries and their id.
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
%% @doc Kill a running query specified by id.
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
%% @doc Instructs Cozo to run a compaction job.
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
%% @doc Create a stored relation with the given name and spec. No
%% stored relation with the same name can exist beforehand.
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
%% @doc Similar to :create, except that if the named stored relation
%% exists beforehand, it is completely replaced.
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
%% @doc Put rows from the resulting relation into the named stored
%% relation.
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
%% @doc Update rows in the named stored relation.
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
%% @doc Remove rows from the named stored relation.
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
%% @doc Ensure that rows specified by the output relation and spec
%% exist in the database, and that no other process has written to
%% these rows when the enclosing transaction commits. Useful for
%% ensuring read-write consistency.
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
%% @doc Ensure that rows specified by the output relation and spec do
%% not exist in the database and that no other process has written to
%% these rows when the enclosing transaction commits.
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
%% @doc wrapper around JSON decoder, by default using `thoas'.
%% @end
%%--------------------------------------------------------------------
decode_json(Message) ->
    Decoder = json_decoder(),
    case Decoder:decode(Message) of
	{ok, Decoded} -> {ok, Decoded};
	{error, Error} -> {error, {Error, Message}};
	Elsewise -> Elsewise
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
      Path :: string(),
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
