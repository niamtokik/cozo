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
%%%===================================================================
-module(cozo_nif_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("cozo.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> [simple, export_import, backup_restore].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
simple() -> [].
simple(Config) ->
    Modes = proplists:get_value(modes, Config, [normal,debug]),
    DbPath = "/tmp/cozodb_nif_test.db\n",
    DbOptions = "{}\n",
    Query = "?[] <- [[1,2,3]]\n",
    QueryParams = "{}\n",
    lists:map(fun(Mode) ->
		      case Mode of
			  debug -> application:set_env(cozo, debug, true);
			  _ -> ok
		      end,
		      {ok, Db} = cozo_nif:open_db("mem\n", DbPath, DbOptions),
		      {ok, R1} = cozo_nif:run_query(Db, Query, QueryParams, 0),
		      ct:pal(info, ?LOW_IMPORTANCE, "run_query: ~p", [R1]),
		      ok = cozo_nif:close_db(Db),
		      case Mode of
			  debug -> application:set_env(cozo, debug, false);
			  _ -> ok
		      end
	      end, Modes).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
export_import() -> [].
export_import(_Config) ->
    DbPath = "/tmp/cozodb_nif_test.db\n",
    DbOptions = "{}\n",
    {ok, Db} = cozo_nif:open_db("mem\n", DbPath, DbOptions),

    Relations = "{\"stored\":{\"headers\":[\"c1\",\"c2\"],\"rows\":\[\]}}\n",
    {ok, R1} = cozo_nif:import_relations_db(Db, Relations),
    ct:pal(info, ?LOW_IMPORTANCE, "import_relations_db: ~p", [R1]),

    RelationsToExport = "{\"relations\":[\"stored\"]}\n",
    {ok, R2} = cozo_nif:export_relations_db(Db, RelationsToExport),
    ct:pal(info, ?LOW_IMPORTANCE, "export_relations_db: ~p", [R2]),

    ok = cozo_nif:close_db(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
backup_restore() -> [].
backup_restore(_Config) ->
    DbPath = "/tmp/cozodb_nif_test.db\n",
    DbOptions = "{}\n",
    BackupPath = "/tmp/cozodb_backup_test.db\n",
    BackupResult = "{\"ok\":true}",
    RestoreResult = "{\"ok\":true}",

    {ok, Db} = cozo_nif:open_db("mem\n", DbPath, DbOptions),
    {ok, BackupResult} = cozo_nif:backup_db(Db, BackupPath),
    ct:pal(info, ?LOW_IMPORTANCE, "backup_db: ~p", [BackupResult]),

    {ok, RestoreResult} = cozo_nif:restore_db(Db, BackupPath),
    ct:pal(info, ?LOW_IMPORTANCE, "restore_db: ~p", [RestoreResult]),

    ok = cozo_nif:close_db(Db),
    file:delete(BackupPath).
