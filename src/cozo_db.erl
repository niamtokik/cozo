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
%%% @doc Isolated cozodb in gen_statem process.
%%%
%%% == Standard ==
%%%
%%% ```
%%% '''
%%%
%%% == Master/Slave ==
%%%
%%% ```
%%%                     ________
%%%  ________          |  ______|_
%%% |        |   +---->| |  ______|_
%%% | master |--(*)----->| |        |
%%% |________|   +-------->| slaves |
%%%                        |________|
%%%
%%%  master: read-write/all
%%%  slave: read-write/master, read-only/all
%%%
%%% '''
%%%
%%% == Replicated ==
%%%
%%% ```
%%% '''
%%%
%%% == Distributed ==
%%%
%%% ```
%%% '''
%%%
%%% @end
%%%===================================================================
-module(cozo_db).
-behavior(gen_statem).

% process management.
-export([start/1, start/2]).
-export([start_monitor/1, start_monitor/2]).
-export([start_link/1, start_link/2]).
-export([stop/1]).

% gen_statem API.
-export([get_path/1, get_id/1, get_options/1, get_engine/1]).
-export([call/2, call/3, cast/2]).

% cozo API
-export([open/0, open/1, open/2, open/3]).
-export([close/1]).
-export([run/2,run/3,run/4]).
-export([import_relations/2, export_relations/2]).
-export([backup/2, restore/2, import_backup/2]).
-export([list_relations/1]).
-export([delete_relation/2, delete_relations/2]).
-export([create_relation/3, replace_relation/3]).
-export([put_row/3, update_row/3, delete_row/3]).
-export([ensure_row/3, ensure_not_row/3]).
-export([list_indices/2, create_index/3, delete_index/2]).
-export([get_triggers/2, set_triggers/3]).
-export([create_hnsw/3, delete_hnsw/2]).
-export([create_lsh/3, delete_lsh/2]).
-export([create_fts/3, delete_fts/2]).
-export([list_columns/2]).
-export([explain/2, describe/3]).
-export([set_access_level/3, set_access_levels/3]).
-export([get_running_queries/1, kill/2, compact/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([standard/3]).

% extra includes
-include_lib("kernel/include/logger.hrl").
-include("cozo.hrl").

% default parameter
-define(DEFAULT_OPEN, start).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start(Args) -> start(Args, []).
start(Args, Opts) -> gen_statem:start(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_monitor(Args) -> start_monitor(Args, []).
start_monitor(Args, Opts) -> gen_statem:start_monitor(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link(Args) -> start_link(Args, []).
start_link(Args, Opts) -> gen_statem:start_link(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
stop(Pid) -> gen_statem:stop(Pid).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
call(Pid, Message) -> call(Pid, Message, 1000).
call(Pid, Message, Timeout) -> gen_statem:call(Pid, Message, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
cast(Pid, Message) -> gen_statem:cast(Pid, Message).

%%--------------------------------------------------------------------
%% @doc
%% @see start/1
%% @see cozo:open/0
%% @end
%%--------------------------------------------------------------------
open() -> ?DEFAULT_OPEN([]).

%%--------------------------------------------------------------------
%% @doc
%% @see start/1
%% @see cozo:open/1
%% @end
%%--------------------------------------------------------------------
open(Engine) -> ?DEFAULT_OPEN([{engine, Engine}]).

%%--------------------------------------------------------------------
%% @doc
%% @see start/1
%% @see cozo:open/2
%% @end
%%--------------------------------------------------------------------
open(Engine, Path) ->
    ?DEFAULT_OPEN([{engine, Engine}
		  ,{path, Path}
		  ]).

%%--------------------------------------------------------------------
%% @doc
%% @see start/1
%% @see cozo:open/3
%% @end
%%--------------------------------------------------------------------
open(Engine, Path, DbOptions) ->
    ?DEFAULT_OPEN([{engine, Engine}
                  ,{path, Path}
                  ,{options, DbOptions}
                  ]).

%%--------------------------------------------------------------------
%% @doc
%% @see stop/1
%% @see cozo:close/1
%% @end
%%--------------------------------------------------------------------
close(Pid) -> cast(Pid, close).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:run/2
%% @end
%%--------------------------------------------------------------------
run(Pid, Query) -> call(Pid, {run, [Query]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:run/3
%% @end
%%--------------------------------------------------------------------
run(Pid, Query, Params) -> call(Pid, {run, [Query, Params]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:run/4
%% @end
%%--------------------------------------------------------------------
run(Pid, Query, Params, Mutable) ->
    call(Pid, {run, [Query, Params, Mutable]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:import_relations/2
%% @end
%%--------------------------------------------------------------------
import_relations(Pid, Json) ->
    call(Pid, {import_relations, [Json]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:export_relations/2
%% @end
%%--------------------------------------------------------------------
export_relations(Pid, Json) ->
    call(Pid, {export_relations, [Json]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:backup/2
%% @end
%%--------------------------------------------------------------------
backup(Pid, OutPath) ->
    call(Pid, {backup, [OutPath]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:restore/2
%% @end
%%--------------------------------------------------------------------
restore(Pid, InPath) ->
    call(Pid, {restore, [InPath]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:import_backup/2
%% @end
%%--------------------------------------------------------------------
import_backup(Pid, Json) ->
    call(Pid, {import_backup, [Json]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:list_relations/1
%% @end
%%--------------------------------------------------------------------
list_relations(Pid) ->
    call(Pid, {list_relations, []}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:explain/2
%% @end
%%--------------------------------------------------------------------
explain(Pid, Query) ->
    call(Pid, {explain, [Query]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:list_columns/2
%% @end
%%--------------------------------------------------------------------
list_columns(Pid, Name) ->
    call(Pid, {list_columns, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:list_indices/2
%% @end
%%--------------------------------------------------------------------
list_indices(Pid, Name) ->
    call(Pid, {list_indices, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:describe/3
%% @end
%%--------------------------------------------------------------------
describe(Pid, Name, Description) ->
    call(Pid, {describe, [Name, Description]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:remove_relation/2
%% @end
%%--------------------------------------------------------------------
delete_relation(Pid, Name) ->
    call(Pid, {delete_relation, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:remove_relation/2
%% @end
%%--------------------------------------------------------------------
delete_relations(Pid, Names) ->
    call(Pid, {delete_relations, [Names]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:get_triggers/2
%% @end
%%--------------------------------------------------------------------
get_triggers(Pid, Name) ->
    call(Pid, {get_triggers, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:set_access_level/3
%% @end
%%--------------------------------------------------------------------
set_access_level(Pid, Level, Name) ->
    call(Pid, {set_access_level, [Level, Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:set_access_levels/3
%% @end
%%--------------------------------------------------------------------
set_access_levels(Pid, Level, Names) ->
    call(Pid, {set_access_levels, [Level, Names]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:get_running_queries/1
%% @end
%%--------------------------------------------------------------------
get_running_queries(Pid) ->
    call(Pid, {get_running_queries, []}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:kill/2
%% @end
%%--------------------------------------------------------------------
kill(Pid, Id) ->
    call(Pid, {kill, [Id]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:compact/1
%% @end
%%--------------------------------------------------------------------
compact(Pid) ->
    call(Pid, {compact, []}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:create_relation/3
%% @end
%%--------------------------------------------------------------------
create_relation(Pid, Name, Spec) ->
    call(Pid, {create_relation, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:replace_relation/3
%% @end
%%--------------------------------------------------------------------
replace_relation(Pid, Name, Spec) ->
    call(Pid, {replace_relation, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:put_row/3
%% @end
%%--------------------------------------------------------------------
put_row(Pid, Name, Spec) ->
    call(Pid, {put_row, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:update_row/3
%% @end
%%--------------------------------------------------------------------
update_row(Pid, Name, Spec) ->
    call(Pid, {update_row, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:delete_row/3
%% @end
%%--------------------------------------------------------------------
delete_row(Pid, Name, Spec) ->
    call(Pid, {delete_row, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:ensure_row/3
%% @end
%%--------------------------------------------------------------------
ensure_row(Pid, Name, Spec) ->
    call(Pid, {ensure_row, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:ensure_not_row/3
%% @end
%%--------------------------------------------------------------------
ensure_not_row(Pid, Name, Spec) ->
    call(Pid, {ensure_not_row, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:create_index/3
%% @end
%%--------------------------------------------------------------------
create_index(Pid, Name, Spec) ->
    call(Pid, {create_index, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:delete_index/2
%% @end
%%--------------------------------------------------------------------
delete_index(Pid, Name) ->
    call(Pid, {delete_index, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:set_triggers/3
%% @end
%%--------------------------------------------------------------------
set_triggers(Pid, Name, Spec) ->
    call(Pid, {set_triggers, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:create_hnsw/3
%% @end
%%--------------------------------------------------------------------
create_hnsw(Pid, Name, Spec) ->
    call(Pid, {create_hnsw, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:delete_index/2
%% @end
%%--------------------------------------------------------------------
delete_hnsw(Pid, Name) ->
    call(Pid, {delete_hnsw, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:create_lsh/3
%% @end
%%--------------------------------------------------------------------
create_lsh(Pid, Name, Spec) ->
    call(Pid, {create_lsh, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:delete_lsh/2
%% @end
%%--------------------------------------------------------------------
delete_lsh(Pid, Name) ->
    call(Pid, {delete_lsh, [Name]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:create_fts/3
%% @end
%%--------------------------------------------------------------------
create_fts(Pid, Name, Spec) ->
    call(Pid, {create_fts, [Name, Spec]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:delete_fts/2
%% @end
%%--------------------------------------------------------------------
delete_fts(Pid, Name) ->
    call(Pid, {delete_fts, [Name]}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Engine = proplists:get_value(engine, Args, mem),
    DbPath = application:get_env(cozo, db_path, "/tmp"),
    DbPrefix = application:get_env(cozo, db_filename_prefix, "cozodb_server_"),
    Path = case proplists:get_value(path, Args) of
               undefined ->
                   cozo:create_filepath(DbPath, DbPrefix, 32);
               Elsewise -> 
                   Elsewise
           end,
    Options = proplists:get_value(options, Args, #{}),
    case cozo:open(Engine, Path, Options) of
        {ok, State} ->
            {ok, standard, State};
        {error, Error} ->
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Returns the path used by the database.
%% @end
%%--------------------------------------------------------------------
get_path(Pid) -> call(Pid, {get, path}).

%%--------------------------------------------------------------------
%% @doc Returns database id.
%% @end
%%--------------------------------------------------------------------
get_id(Pid) -> call(Pid, {get, id}).

%%--------------------------------------------------------------------
%% @doc Returns the options used by the database.
%% @end
%%--------------------------------------------------------------------
get_options(Pid) -> call(Pid, {get, options}).

%%--------------------------------------------------------------------
%% @doc Returns the engine used by the database.
%% @end
%%--------------------------------------------------------------------
get_engine(Pid) -> call(Pid, {get, engine}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
standard(cast, close, #cozo{ id = Id } = _Data) ->
    cozo:close(Id),
    {stop, normal};
standard({call, From}, {get, path}, #cozo{ db_path = Path } = Data) ->
    {keep_state, Data, [{reply, From, Path}]};
standard({call, From}, {get, id}, #cozo{ id = Id } = Data) ->
    {keep_state, Data, [{reply, From, Id}]};
standard({call, From}, {get, options}, #cozo{ db_options = Options } = Data) ->
    {keep_state, Data, [{reply, From, Options}]};
standard({call, From}, {get, engine}, #cozo{ db_engine = Engine } = Data) ->
    {keep_state, Data, [{reply, From, Engine}]};
standard({call, From}, {Function, Args}, Data)
  when is_atom(Function) andalso is_list(Args) ->
    query(Function, Args, From, Data);
standard(EventType, EventContent, Data) ->
    Args = [EventType, EventContent],
    ?LOG_WARNING("received: ~p", [{self(), ?MODULE, standard, Args}]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
query(Function, Args, From, #cozo{ id = Id } = Data)
  when is_atom(Function) andalso is_list(Args) ->
    try
        CallArgs = [Id|Args],
        Return = erlang:apply(cozo, Function, CallArgs),
        ?LOG_DEBUG("~p", [{self(), ?MODULE, query, CallArgs, Return}]),
        {keep_state, Data, [{reply, From, Return}]}
    catch
        Error:Reason ->
            {keep_state, Data, [{reply, From, {Error, Reason}}]}
    end;
query(Function, Args, From, Data) ->
    ?LOG_ERROR("~p", [{self(), ?MODULE, query, Function, Args}]),
    {keep_state, Data, [{reply, From, {error, {Function, Args}}}]}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State, #cozo{ id = Id } = _Data) ->
    cozo:close(Id).
