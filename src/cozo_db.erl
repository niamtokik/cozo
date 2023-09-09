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
%%% @end
%%%===================================================================
-module(cozo_db).
-behavior(gen_statem).

% process management.
-export([start/1, start/2]).
-export([start_monitor/1, start_monitor/2]).
-export([start_link/1, start_link/2]).
-export([stop/1]).

% API.
-export([call/2, call/3, cast/2]).
-export([run/2, run/3, run/4]).
-export([import_relations/2, export_relations/2]).
-export([backup/2, restore/2, import_backup/2]).
-export([list_relations/1, explain/2]).
-export([list_columns/2, list_indices/2]).
-export([describe/3, remove_relation/2, get_triggers/2]).
-export([set_access_level/3, set_access_levels/3]).
-export([get_running_queries/1, kill/2, compact/1]).
-export([create_relation/3, replace_relation/3]).
-export([put_row/3, update_row/3, remove_row/3]).
-export([ensure_row/3, ensure_not_row/3]).

% Internal functions
-export([callback_mode/0, init/1, terminate/3]).
-export([standard/3]).
-include_lib("kernel/include/logger.hrl").
-include("cozo.hrl").

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
%% @see cozo:run/2
%% @end
%%--------------------------------------------------------------------
run(Pid, Query) ->
    call(Pid, {run, [Query]}).

%%--------------------------------------------------------------------
%% @doc
%% @see cozo:run/3
%% @end
%%--------------------------------------------------------------------
run(Pid, Query, Params) ->
    call(Pid, {run, [Query, Params]}).

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
remove_relation(Pid, Name) ->
    call(Pid, {remove_relation, [Name]}).

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
%% @see cozo:remove_row/3
%% @end
%%--------------------------------------------------------------------
remove_row(Pid, Name, Spec) ->
    call(Pid, {remove_row, [Name, Spec]}).

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
    Path = case proplists:get_value(path, Args) of
	       undefined ->
		   cozo:create_filepath("/tmp", "cozodb_server_", 32);
	       Elsewise -> Elsewise
	   end,
    Options = proplists:get_value(options, Args, #{}),
    case cozo:open(Engine, Path, Options) of
	{ok, {_Id, State}} ->
	    {ok, standard, State};
	{error, Error} ->
	    {stop, Error}
    end.

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
