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
-module(cozo_db_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("cozo.hrl").
-include("cozo_test.hrl").
-define(DB_PATH_TEST, "/tmp/cozo_db_test").
-define(DB_PREFIX, "cozodb_test_SUITE").
-define(MODULE_TEST, cozo_db).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() -> [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:set_env(cozo, db_path, ?DB_PATH_TEST),
    ok = application:set_env(cozo, db_filename_prefix, ?DB_PREFIX),
    case proplists:get_value(create_path, Config, true) of
        true -> 
            ok = file:make_dir(?DB_PATH_TEST);
        _ -> ok
    end,
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    case proplists:get_value(clean_path, Config, true) of
        false -> ok;
        _ -> ok = file:del_dir_r(?DB_PATH_TEST)
    end,
    ok = application:unset_env(cozo, db_filename_prefix),
    ok = application:unset_env(cozo, db_path),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) -> Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) -> ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) -> Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) -> ok.

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
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> [simple].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
simple() -> [].

simple(_Config) ->
    % open a new database
    {ok, Db} = cozo_db:open(),

    % query using string
    % {ok, _} = cozo_db:run(Db, "?[] <- [[1, 2, 3]]"),
    {ok, _} = cozo_db:run(Db, "?[] <- [[1, 2, 3]]"),

    % query using binary
    {ok, _} = cozo_db:run(Db, <<"?[] <- [[1, 2, 3]]">>),

    % query using a list of string
    {ok, _} = cozo_db:run(Db, ["?[] <- [[1, 2, 3]]"]),

    % wrong term used for queries
    {error, _} = cozo_db:run(Db, query),
    {error, _} = cozo_db:run(Db, ""),
    {error, _} = cozo_db:run(Db, "", ""),

    % close database
    ok = cozo_db:close(Db).

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
multi_spawn() -> [].
     
multi_spawn(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Counters = [100, 1_000, 10_000],
    Methods = [start, start_link, start_monitor],
    Mapping = [ {C,M,E} || C <- Counters, M <- Methods, E <- Engines ],
    lists:map(fun cozo_spawn_fun/1, Mapping).

cozo_spawn_fun({Counter, Method, Engine}) ->
    Opts = [{engine, Engine}],
    Open = fun() -> {ok, Db} = cozo_db:Method(Opts), Db end,
    Dbs = [ Open() || _ <- lists:seq(1, Counter) ],
    Run = fun(Db) -> spawn(cozo_db, run, [Db, "?[] <- [[1, 2, 3]]"]) end,
    [ Run(Db) || Db <- Dbs ],
    [ cozo_db:stop(Db) || Db <- Dbs ].
