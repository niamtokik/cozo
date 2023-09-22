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
-module(cozo_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("cozo.hrl").
-include("cozo_test.hrl").
-define(DB_PATH_TEST, "/tmp/cozo_test").
-define(DB_PREFIX, "cozodb_test_SUITE").
-define(MODULE_TEST, cozo).

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

all() -> [ simple
   , tutorial_intro, tutorial_expressions, tutorial_rules
         , tutorial_stored_relations, tutorial_command_blocks
         , tutorial_graphs, air_routes, multi_spawn
   , maintenance_commands, system_commands
   , index, hnsw, lsh, fts
   , sqlite, rocksdb].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
simple() -> [].

simple(_Config) ->
    % open a new database
    {ok, Db} = cozo:open(),

    % query using string
    % {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),
    {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),

    % query using binary
    {ok, _} = cozo:run(Db, <<"?[] <- [[1, 2, 3]]">>),

    % query using a list of string
    {ok, _} = cozo:run(Db, ["?[] <- [[1, 2, 3]]"]),

    % wrong term used for queries
    {error, _} = cozo:run(Db, query),
    {error, _} = cozo:run(Db, ""),
    {error, _} = cozo:run(Db, "", ""),

    % close database
    ok = cozo:close(Db),

    % wrong engine
    {error, {bad_engine, _}} = cozo:open(test),

    % not existing path
    % does not work correctly on github workflow
    % {error, _} = cozo:open(mem, "/not/existing/path"),

    % bad path
    {error, _} = cozo:open(mem, a_wrong_path),

    % bad configuration provided
    {error, _} = cozo:open(mem, "/tmp", "bad"),

    % close an already closed database
    {error, _} = cozo:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#First-steps
%%--------------------------------------------------------------------
tutorial_intro() -> [].

tutorial_intro(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_intro_fun/1, Mapping).

tutorial_intro_fun({Module, Engine}) ->
    ?QUERY_OK("?[] <- [['hello', 'world', 'Cozo!']]"),
    ?QUERY_OK("?[] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK("?[] <- [[1.5, 2.5, 3, 4, 5.5],"
              "['aA', 'bB', 'cC', 'dD', 'eE'],"
              "[true, false, null, -1.4e-2, \"A string with double quotes\"]]"),
    ?QUERY_OK("?[] <- [[1], [2], [1], [2], [1]]").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Expressions
%%--------------------------------------------------------------------
tutorial_expressions() -> [].

tutorial_expressions(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_expressions_fun/1, Mapping).

tutorial_expressions_fun({Module, Engine}) ->
    ?QUERY_OK("?[] <- [["
              "1 + 2,"
              "3 / 4,"
              "5 == 6,"
              "7 > 8,"
              "true || false,"
              "false && true,"
              "lowercase('HELLO'),"
              "rand_float(),"
              "union([1, 2, 3], [3, 4, 5], [5, 6, 7])"
              "]]"),
    ?QUERY_OK("a[x, y] <- [[1, 2], [3, 4]]"
              "b[y, z] <- [[2, 3], [2, 4]]"
              "?[x, y, z] := a[x, y], b[y, z]"
              "?[x, y, z] := a[x, y], not b[y, _], z = null").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
%%--------------------------------------------------------------------
tutorial_rules() -> [].

tutorial_rules(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_rules_fun/1, Mapping).

tutorial_rules_fun({Module, Engine}) ->
    ?QUERY_OK("?[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK("rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[a, b, c] := rule[a, b, c]"),
    ?QUERY_OK("rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b] := rule[a, b, c]"),
    ?QUERY_OK("?[c, b] := rule[a, b, c], is_num(a)"
              "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK("rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b] := rule['a', b, c]"),
    ?QUERY_OK("rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b, d] := rule[a, b, c], is_num(a), d = a + b + 2*c"),
    ?QUERY_OK("?[x, y] := x in [1, 2, 3], y in ['x', 'y']").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
%%--------------------------------------------------------------------
tutorial_joins() -> [].

tutorial_joins(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_joins_fun/1, Mapping).

tutorial_joins_fun({Module, Engine}) ->
    ?QUERY_OK("r1[] <- [[1, 'a'], [2, 'b']]"
              "r2[] <- [[2, 'B'], [3, 'C']]"
              "?[l1, l2] := r1[a, l1], r2[b, l2]"),

    ?QUERY_OK("r1[] <- [[1, 'a'], [2, 'b']]"
              "r2[] <- [[2, 'B'], [3, 'C']]"
              "?[l1, l2] := r1[a, l1],"
              "             r2[a, l2]"),

    ?QUERY_OK("a[x, y] <- [[1, 2], [3, 4]]"
              "b[y, z] <- [[2, 3], [2, 4]]"
              "?[x, y, z] := a[x, y], b[y, z]"
              "?[x, y, z] := a[x, y], not b[y, _], z = null").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Stored-relations
%%--------------------------------------------------------------------
tutorial_stored_relations() -> [].

tutorial_stored_relations(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_stored_relations_fun/1, Mapping).

tutorial_stored_relations_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    ?IQUERY_LOG(Db, ":create stored {c1, c2}"),
    ?IQUERY_LOG(Db, ":create dept_info {"
                "company_name: String,"
                "department_name: String,"
                "=>"
                "head_count: Int default 0,"
                "address: String,"
                "}"),
    ?IQUERY_LOG(Db, "?[a, b, c] <- [[1, 'a', 'A'],"
                "[2, 'b', 'B'],"
                "[3, 'c', 'C'],"
                "[4, 'd', 'D']]"),
    ?IQUERY_LOG(Db, ":create fd {a, b => c}"),
    ?IQUERY_LOG(Db, "?[a, b, c] := *fd[a, b, c]"),
    ?IQUERY_LOG(Db, "?[a, b, c] <- [[3, 'c', 'CCCCCCC']]"),
    ?IQUERY_LOG(Db, ":put fd {a, b => c}"
                "?[a, b, c] := *fd[a, b, c]"),
    ?IQUERY_LOG(Db, "::relations"),
    ?IQUERY_LOG(Db, "::columns stored"),

    % @todo: crash ?IQUERY_LOG(Db, "?[a, b] := *stored[a, b]"),
    % @todo: crash ?IQUERY_LOG(Db, "?[a, b] := *stored{l2: b, l1: a}"),
    % @todo: crash ?IQUERY_LOG(Db, "?[l2] := *stored{l2}"),
    % ?IQUERY_LOG(Db, "?[l1, l2] <- [['e', 'E']]"
    %		":rm stored {l1, l2}"),

    ?IQUERY_LOG(Db, "?[l1, l2] := *stored[l1, l2]"),
    ?IQUERY_LOG(Db, "::remove stored"),
    ?IQUERY_LOG(Db, "::relations"),
    ?IQUERY_LOG(Db, "::remove fd"),
    ok = Module:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Command-blocks
%%--------------------------------------------------------------------
tutorial_command_blocks() -> [].

tutorial_command_blocks(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_command_blocks_fun/1, Mapping).

tutorial_command_blocks_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    ?IQUERY_LOG(Db, "{?[a] <- [[1], [2], [3]]; :replace test {a}}"
                "{?[a] <- []; :replace test2 {a}}"
                "%swap test test2"
                "%return test"),
    ok = Module:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Graphs
%%--------------------------------------------------------------------
tutorial_graphs() -> [].

tutorial_graphs(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun tutorial_graphs_fun/1, Mapping).

tutorial_graphs_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    ?IQUERY_LOG(Db, "?[loving, loved] <- [['alice', 'eve'],"
                "['bob', 'alice'],"
                "['eve', 'alice'],"
                "['eve', 'bob'],"
                "['eve', 'charlie'],"
                "['charlie', 'eve'],"
                "['david', 'george'],"
                "['george', 'george']]"
                ":replace love {loving, loved}"),

    ?IQUERY_LOG(Db, "?[loved_by_b_e] := *love['eve', loved_by_b_e],"
                "*love['bob', loved_by_b_e]"),

    ?IQUERY_LOG(Db, "?[loved_by_b_e] := *love['eve', loved_by_b_e] or *love['bob', loved_by_b_e],"
                "loved_by_b_e != 'bob',"
                "loved_by_b_e != 'eve'"),

    ?IQUERY_LOG(Db, "?[loved_by_b_e] := *love['eve', loved_by_b_e],"
               "loved_by_b_e != 'bob',"
               "loved_by_b_e != 'eve'"
               "?[loved_by_b_e] := *love['bob', loved_by_b_e],"
               "loved_by_b_e != 'bob',"
               "loved_by_b_e != 'eve'"),

    ?IQUERY_LOG(Db, "?[loved] := *love[person, loved], !ends_with(person, 'e')"),

    ?IQUERY_LOG(Db, "?[loved_by_e_not_b] := *love['eve', loved_by_e_not_b],"
                "not *love['bob', loved_by_e_not_b]"),

    % @todo: crash to fix
    % ?IQUERY_LOG(Db, "?[not_loved_by_b] := not *love['bob', not_loved_by_b]"),

    ?IQUERY_LOG(Db, "the_population[p] := *love[p, _a]"
                "the_population[p] := *love[_a, p]"
                "?[not_loved_by_b] := the_population[not_loved_by_b],"
                "not *love['bob', not_loved_by_b]"),

    ?IQUERY_LOG(Db, "alice_love_chain[person] := *love['alice', person]"
                "alice_love_chain[person] := alice_love_chain[in_person],"
                "*love[in_person, person]"
                "?[chained] := alice_love_chain[chained]"),

    ?IQUERY_LOG(Db, "alice_love_chain[person] := alice_love_chain[in_person],"
                "*love[in_person, person]"
                "?[chained] := alice_love_chain[chained]"),

    ?IQUERY_LOG(Db, "?[loving, loved] := *love{ loving, loved }"
                ":limit 1"),

    ?IQUERY_LOG(Db, "?[loving, loved] := *love{ loving, loved }"
                ":order -loved, loving"
                ":offset 1"),

    ?IQUERY_LOG(Db, "?[loving, loved] := *love{ loving, loved }"
                ":limit 1"),

    ?IQUERY_LOG(Db, "?[loving, loved] := *love{ loving, loved }\n"
                ":order -loved, loving\n"
                ":offset 1"),

    ?IQUERY_LOG(Db, "?[] <~ Constant(data: [['hello', 'world', 'Cozo!']])"),

    ?IQUERY_LOG(Db, "?[person, page_rank] <~ PageRank(*love[])\n"
                ":order -page_rank"),

    ?IQUERY_LOG(Db, "::remove love"),
    ok = Module:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Extended-example:-the-air-routes-dataset
%%--------------------------------------------------------------------
air_routes() -> [].

air_routes(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {DataDir, M,E} || M <- Modules, E <- Engines ],
    lists:map(fun air_routes_fun/1, Mapping).

air_routes_fun({DataDir, Module, Engine}) ->
    {ok, RawAirRoutes} = file:read_file(filename:join(DataDir, "air-routes.json")),
    {ok, AirRoutes} = thoas:decode(RawAirRoutes),
    {ok, Db} = Module:open(Engine),
    ?IQUERY_LOG(Db, "{:create airport {"
                "code: String"
                "=>"
                "icao: String,"
                "desc: String,"
                "region: String,"
                "runways: Int,"
                "longest: Float,"
                "elev: Float,"
                "country: String,"
                "city: String,"
                "lat: Float,"
                "lon: Float"
                "}}"),

    ?IQUERY_LOG(Db, "{:create country {"
                "code: String"
                "=>"
                "desc: String"
                "}}"),

    ?IQUERY_LOG(Db, "{:create continent {"
                "code: String"
                "=>"
                "desc: String"
                "}}"),

    ?IQUERY_LOG(Db, "{:create contain { entity: String, contained: String }}"),

    ?IQUERY_LOG(Db, "{:create route { fr: String, to: String => dist: Float }}"),

    Module:import_backup(Db, AirRoutes),

    ?IQUERY_LOG(Db, "::relations"),

    ?IQUERY_LOG(Db, "?[code, city, desc, region, runways, lat, lon]"
                ":= *airport{code, city, desc, region, runways, lat, lon}"
                ":limit 5"),

    ?IQUERY_LOG(Db, "?[code, city, desc, region, runways, lat, lon]"
                ":= *airport{code, city, desc, region, runways, lat, lon}"
                ":order -runways"
                ":limit 10"),
    
    ?IQUERY_LOG(Db, "?[count(code)] := *airport{code}"),

    ?IQUERY_LOG(Db, "?[count(initial), initial]"
                ":= *airport{code}, initial = first(chars(code))"
                ":order initial"),

    ?IQUERY_LOG(Db, "?[count(r), count_unique(r), sum(r), min(r), max(r), mean(r), std_dev(r)]"
                ":= *airport{runways: r}"),

    ?IQUERY_LOG(Db, "?[desc] := *country{code, desc}, not *airport{country: code}"),

    ?IQUERY_LOG(Db, "?[fr, to, dist] := *route{fr, to, dist}"
                ":limit 10"),

    ?IQUERY_LOG(Db, "?[code, desc] := *airport{code, desc}, not *route{fr: code}, not *route{to: code}"),

    ?IQUERY_LOG(Db, "route_count[fr, count(fr)] := *route{fr}"
                "?[code, n] := route_count[code, n]"
                ":sort -n"
                ":limit 5"),

    ?IQUERY_LOG(Db, "routes[unique(r)] := *contain['EU', fr],"
                "*route{fr, to},"
                "*airport{code: to, country: 'US'},"
                "r = [fr, to]"
                "?[n] := routes[rs], n = length(rs)"),

    ?IQUERY_LOG(Db, "?[count_unique(to)] := *contain['EU', fr],"
                "*route{fr, to},"
                "*airport{code: to, country: 'US'}"),

    ?IQUERY_LOG(Db, "?[code, count(code)]"
                ":= *airport{code, city: 'London', region: 'GB-ENG'}, *route{fr: code}"),

    ?IQUERY_LOG(Db, "lon_uk_airports[code] := *airport{code, city: 'London', region: 'GB-ENG'}"
                "one_hop[to] := lon_uk_airports[fr], *route{fr, to}, not lon_uk_airports[to];"
                "?[count_unique(a3)] := one_hop[a2], *route{fr: a2, to: a3}, not lon_uk_airports[a3];"),

    ?IQUERY_LOG(Db, "?[city, dist] := *route{fr: 'LGW', to, dist},"
                "*airport{code: to, city}"
                ":order -dist"
                ":limit 10"),

    ?IQUERY_LOG(Db, "?[code, desc, lon, lat] := *airport{lon, lat, code, desc}, lon > -0.1, lon < 0.1"),

    ?IQUERY_LOG(Db, "h_box[lon, lat] := *airport{code: 'LHR', lon, lat}"
                "?[code, desc] := h_box[lhr_lon, lhr_lat], *airport{code, lon, lat, desc},"
                "abs(lhr_lon - lon) < 1, abs(lhr_lat - lat) < 1"),

    ?IQUERY_LOG(Db, "?[deg_diff] := *airport{code: 'SFO', lat: a_lat, lon: a_lon},"
                "*airport{code: 'NRT', lat: b_lat, lon: b_lon},"
                "deg_diff = rad_to_deg(haversine_deg_input(a_lat, a_lon, b_lat, b_lon))"),

    ?IQUERY_LOG(Db, "shortest[b, min(dist)] := *route{fr: 'LHR', to: b, dist}"
                "shortest[b, min(dist)] := shortest[c, d1],"
                "*route{fr: c, to: b, dist: d2},"
                "dist = d1 + d2"
                "?[dist] := shortest['YPO', dist]"),

    ?IQUERY_LOG(Db, "shortest[a, b, min(dist)] := *route{fr: a, to: b, dist}"
                "shortest[a, b, min(dist)] := shortest[a, c, d1],"
                "*route{fr: c, to: b, dist: d2},"
                "dist = d1 + d2"
                "?[dist] := shortest['LHR', 'YPO', dist]"),

    ?IQUERY_LOG(Db, "starting[] <- [['LHR']]"
                "goal[] <- [['YPO']]"
                "?[starting, goal, distance, path] <~ ShortestPathDijkstra(*route[], starting[], goal[])"),

    ?IQUERY_LOG(Db, "starting[] <- [['LHR']]"
                "goal[] <- [['YPO']]"
                "?[starting, goal, distance, path] <~ KShortestPathYen(*route[], starting[], goal[], k: 10)"),

    ?IQUERY_LOG(Db, "code_lat_lon[code, lat, lon] := *airport{code, lat, lon}"
                "starting[code, lat, lon] := code = 'LHR', *airport{code, lat, lon};"
                "goal[code, lat, lon] := code = 'YPO', *airport{code, lat, lon};"
                "?[] <~ ShortestPathAStar(*route[],"
                "code_lat_lon[node, lat1, lon1],"
                "starting[],"
                "goal[goal, lat2, lon2],"
                "heuristic: haversine_deg_input(lat1, lon1, lat2, lon2) * 3963);"),

    ?IQUERY_LOG(Db, "rank[code, score] <~ PageRank(*route[a, b])"
                "?[code, desc, score] := rank[code, score], *airport{code, desc}"
                ":limit 10;"
                ":order -score"),

    %% @todo: fix crash:
    %% thread '<unnamed>' panicked at 'index out of bounds: the len is 0 but the index is 0', cozo-core/src/fixed_rule/algos/all_pairs_shortest_path.rs:80:24
    %% note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
    %% fatal runtime error: failed to initiate panic, error 5
    %% Aborted
    %% ?IQUERY_LOG(Db, "centrality[code, score] <~ BetweennessCentrality(*route[a, b])"
    %%    "?[code, desc, score] := centrality[code, score], *airport{code, desc}"
    %%    ":limit 10;"
    %%    ":order -score"),

    ?IQUERY_LOG(Db, "community[detailed_cluster, code] <~ CommunityDetectionLouvain(*route[a, b])"
                "?[code, cluster, detailed_cluster] := community[detailed_cluster, code], cluster = first(detailed_cluster)"
                ":replace community {code => cluster, detailed_cluster}"),

    ?IQUERY_LOG(Db, "community[code] := *community{code: 'LGW', cluster}, *community{code, cluster}"
                "?[country, count(code)] :="
                "community[code],"
                "*airport{code, desc, country: country_code},"
                "*country{code: country_code, desc: country},"
                ":order -count(code)"
                ":limit 5"),

    ?IQUERY_LOG(Db, "community[code] := *community{code: 'JFK', cluster}, *community{code, cluster}"
                "?[country, count(code)] :="
                "community[code],"
                "*airport{code, desc, country: country_code},"
                "*country{code: country_code, desc: country},"
                ":order -count(code)"
                ":limit 5"),

    ?IQUERY_LOG(Db, "?[desc, country_desc] := *airport{code: 'FRA', desc, country: country_code}, *country{code: country_code, desc: country_desc}"),

    ?IQUERY_LOG(Db, "community[code] := *community{code: 'FRA', cluster}, *community{code, cluster}"
                "?[country, count(code)] :="
                "community[code],"
                "*airport{code, desc, country: country_code},"
                "*country{code: country_code, desc: country},"
                ":order -count(code)"
                ":limit 5"),

    ?IQUERY_LOG(Db, "community[code] := *community{code: 'SIN', cluster}, *community{code, cluster}"
                "?[country, count(code)] :="
                "community[code],"
                "*airport{code, desc, country: country_code},"
                "*country{code: country_code, desc: country},"
                ":order -count(code)"
                ":limit 5"),

    ?IQUERY_LOG(Db, "?[desc, country_desc] := *airport{code: 'SIN', desc, country: country_code}, *country{code: country_code, desc: country_desc}"),

    ?IQUERY_LOG(Db, "?[fr_cluster, to_cluster, count(dist), sum(dist)] := *route{fr, to, dist},"
                "*community{code: fr, cluster: fr_cluster},"
                "*community{code: to, cluster: to_cluster}"
                ":replace super_route {fr_cluster, to_cluster => n_routes=count(dist), total_distance=sum(dist)}"),
    
    ?IQUERY_LOG(Db, "?[fr_cluster, to_cluster, n_routes, total_distance] := *super_route{fr_cluster, to_cluster, n_routes, total_distance}, fr_cluster < 2"),

    ?IQUERY_LOG(Db, "?[cluster, score] <~ PageRank(*super_route[])"
                ":order -score"
                ":limit 5"),

    ok = Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
sqlite() -> [].

sqlite(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun sqlite_fun/1, Mapping).

sqlite_fun({Module, _Engine}) ->
    % create a new cozo database using sqlite
    {ok, Db} = Module:open(sqlite),

    DbPath = Module:get_path(Db),
    true = filelib:is_file(DbPath),
    {ok, _} = Module:run(Db, "?[] <- [[1, 2, 3]]"),
    %% {ok, _} = Module:create_relations(Db, "stored" "{c1, c2}"),
    %% {ok, _} = Module:run(Db, ":create dept_info {"
    %%       "company_name: String,"
    %%       "department_name: String,"
    %%       "=>"
    %%       "head_count: Int default 0,"
    %%       "address: String,"
    %%       "}"),
    %% {ok, _} = Module:run(Db, "?[a, b, c] <- [[1, 'a', 'A'],"
    %%           "[2, 'b', 'B'],"
    %%           "[3, 'c', 'C'],"
    %%           "[4, 'd', 'D']]"),
    Module:close(Db),

    % Reopen the database
    {ok, Db2} = Module:open(sqlite, DbPath),
    DbPath = Module:get_path(Db2),
    true = filelib:is_file(DbPath),
    {ok, _} = Module:run(Db2, "?[] <- [[1, 2, 3]]"),
    {ok, _} = Module:run(Db2, "?[a, b, c] <- [[1, 'a', 'A'],"
           "[2, 'b', 'B'],"
           "[3, 'c', 'C'],"
           "[4, 'd', 'D']]"),
    Module:close(Db2),

    % cleanup the file
    file:delete(DbPath).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
rocksdb() -> [].

rocksdb(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun rocksdb_fun/1, Mapping).

rocksdb_fun({Module, _Engine}) ->
    {ok, Db} = Module:open(rocksdb),
    DbPath = Module:get_path(Db),
    true = filelib:is_dir(DbPath),
    {ok, _} = Module:run(Db, "?[] <- [[1, 2, 3]]"),
    %% {ok, _} = Module:create_relations(Db, "stored", [{c1, c2}]),
    %% {ok, _} = Module:run(Db, ":create dept_info {"
    %%       "company_name: String,"
    %%       "department_name: String,"
    %%       "=>"
    %%       "head_count: Int default 0,"
    %%       "address: String,"
    %%       "}"),
    %% {ok, _} = Module:run(Db, "?[a, b, c] <- [[1, 'a', 'A'],"
    %%           "[2, 'b', 'B'],"
    %%           "[3, 'c', 'C'],"
    %%           "[4, 'd', 'D']]"),
    Module:close(Db),

    {ok, Db2} = Module:open(rocksdb, DbPath),
    DbPath = Module:get_path(Db2),
    true = filelib:is_dir(DbPath),
    {ok, _} = Module:run(Db2, "?[] <- [[1, 2, 3]]"),
    {ok, _} = Module:run(Db2, "?[a, b, c] <- [[1, 'a', 'A'],"
           "[2, 'b', 'B'],"
           "[3, 'c', 'C'],"
           "[4, 'd', 'D']]"),
    Module:close(Db2).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
maintenance_commands() -> [].

maintenance_commands(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun maintenance_commands_fun/1, Mapping).

maintenance_commands_fun({Module, Engine}) ->
    EngineName = atom_to_list(Engine),
    {ok, Db} = Module:open(Engine),
    DbPath = Module:get_path(Db),

    {ok, _} = Module:run(Db, "?[] <- [[1,2,3]]"),

    {ok, _} = Module:create_relation(Db, "stored", "c1"),
    {ok, _} = Module:create_relation(Db, "stored2", ["c1","c2","c3"]),
    {ok, _} = Module:create_relation(Db, stored3, c1),
    {ok, _} = Module:create_relation(Db, stored4, [c1,c2,c3]),
    {ok, _} = Module:create_relation(Db, stored5, ["c1",c2,"c3",c4]),

    % import/export relations
    % wrong json lead to an error
    {error, _} = Module:import_relations(Db, #{ {} => {} }),
    Relations = #{ stored => #{ headers => [c1,c2], rows => [] }},
    {ok, _} = Module:import_relations(Db, Relations),

    % wrong json lead to an error
    {error, _} = Module:export_relations(Db, #{ {} => {} }),
    {ok, _} = Module:export_relations(Db, #{ relations => [stored] }),

    % backup/restore database
    BackupPath = DbPath ++ "_" ++ EngineName ++ ".backup",
    {ok, _} = Module:backup(Db, BackupPath),
    {ok, _} = Module:restore(Db, BackupPath),
    ct:pal(info, ?LOW_IMPORTANCE, "~p", [{BackupPath}]),

    % restore from json
    % {ok, Backup} = file:read_file(BackupPath),
    % BackupJson = binary_to_list(Backup),
    % {ok, _} = Module:import_backup(Db, BackupJson),

    % delete relation one by one
    {ok, _} = Module:delete_relation(Db, "stored"),
    {ok, _} = Module:delete_relation(Db, "stored2"),

    % delete many relations
    {ok, _} = Module:delete_relations(Db, ["stored3", "stored4", "stored5"]),

    ok = Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
system_commands() -> [].

system_commands(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun system_commands_fun/1, Mapping).

system_commands_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    _DbPath = Module:get_path(Db),

    % create relations
    {ok, _} = Module:create_relation(Db, "stored", "c1"),
    {ok, _} = Module:create_relation(Db, "stored2", ["c1","c2","c3"]),
    {ok, _} = Module:create_relation(Db, stored3, c1),
    {ok, _} = Module:create_relation(Db, stored4, [c1,c2,c3]),
    {ok, _} = Module:create_relation(Db, stored5, ["c1",c2,"c3",c4]),

    % list available relations
    {ok, _} = Module:list_relations(Db),

    % explain a query
    {ok, _} = Module:explain(Db, "?[] <- [[1,2,3]]"),

    % list columns and indices
    {ok, _} = Module:list_columns(Db, "stored"),
    {ok, _} = Module:list_indices(Db, "stored"),

    % error during describe
    % {ok, _} = Module:describe(Db, "stored", "test").

    {ok, _} = Module:get_triggers(Db, "stored"),
    {ok, _} = Module:get_running_queries(Db),
    {ok, _} = Module:compact(Db),

    % individual access level
    {ok, _} = Module:set_access_level(Db, hidden, "stored"),
    {ok, _} = Module:set_access_level(Db, read_only, "stored"),
    {ok, _} = Module:set_access_level(Db, protected, "stored"),
    {ok, _} = Module:set_access_level(Db, normal, "stored"),

    % multi set access level
    {ok, _} = Module:set_access_levels(Db, hidden, ["stored2", "stored3"]),
    {ok, _} = Module:set_access_levels(Db, read_only, ["stored2", "stored3"]),
    {ok, _} = Module:set_access_levels(Db, protected, ["stored2", "stored3"]),
    {ok, _} = Module:set_access_levels(Db, normal, ["stored2", "stored3"]),

    % remove relations
    {ok, _} = Module:delete_relation(Db, "stored"),
    {ok, _} = Module:delete_relations(Db, ["stored2", "stored3", "stored4"]),

    % close database
    ok = Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
index() -> [].

index(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun index_fun/1, Mapping).

index_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    {ok, _} = Module:run(Db, ":create r {a => b}"),
    {ok, _} = Module:create_index(Db, "r:idx", "b, a"),
    {ok, _} = Module:list_indices(Db, "r"),
    {ok, _} = Module:delete_index(Db, "r:idx"),
    Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
hnsw() -> [].

hnsw(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun hnsw_fun/1, Mapping).

hnsw_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    {ok, _} = Module:run(Db, ":create table {k: String => v: <F32; 128>}"),
    {ok, _} = Module:create_hnsw(Db, "table:index_name", "{"
                                 "dim: 128, "
                                 "m: 50, "
                                 "dtype: F32, "
                                 "fields: [v], "
                                 "distance: L2, "
                                 "ef_construction: 20, "
                                 "filter: k != 'foo', "
                                 "extend_candidates: false, "
                                 "keep_pruned_connections: false, "
                                 "}"),
    % @todo: fix crash
    % {ok, _} = cozo:delete_hnsw(Db, "table:index_name"),
    Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
lsh() -> [].

lsh(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun lsh_fun/1, Mapping).

lsh_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    {ok, _} = Module:run(Db, ":create table {k: String => v: String?}"),
    {ok, _} = Module:create_lsh(Db, "table:index_name", "{ "
                                "extractor: v, "
                                "extract_filter: !is_null(v), "
                                "tokenizer: Simple, "
                                "filters: [AlphaNumOnly], "
                                "n_perm: 200, "
                                "target_threshold: 0.7, "
                                "n_gram: 3, "
                                "false_positive_weight: 1.0, "
                                "false_negative_weight: 1.0, "
                                "}"),
    % @todo: fix crash
    % {ok, _} = cozo:delete_lsh(Db, "table:index_table"),
    Module:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
fts() -> [].

fts(Config) ->
    Engines = proplists:get_value(engines, Config, [mem, sqlite, rocksdb]),
    Modules = proplists:get_value(modules, Config, [cozo, cozo_db]),
    Mapping = [ {M,E} || M <- Modules, E <- Engines ],
    lists:map(fun fts_fun/1, Mapping).

fts_fun({Module, Engine}) ->
    {ok, Db} = Module:open(Engine),
    {ok, _} = Module:run(Db, ":create table {k: String => v: String?}"),
    {ok, _} = Module:create_fts(Db, "table:index_name", "{"
                                "extractor: v, "
                                "extract_filter: !is_null(v), "
                                "tokenizer: Simple, "
                                "filters: [AlphaNumOnly], "
                                "}"),
    % @todo: fix crash
    % {ok, _} = cozo:delete_fts(Db, "table:index_name"),
    Module:close(Db).

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
multi_spawn() -> [].

multi_spawn(_Config) ->
    [ cozo_spawn(100)
    , cozo_spawn(1_000)
    , cozo_spawn(10_000)
    % , cozo_spawn(100_000)
    ].

cozo_spawn(Counter) ->
  Open = fun() -> {ok, Db} = cozo:open(), Db end,
  Dbs = [ Open() || _ <- lists:seq(1, Counter) ],
  Run = fun(Db) -> spawn(cozo, run, [Db, "?[] <- [[1, 2, 3]]"]) end,
  [ Run(Db) || Db <- Dbs ],
  [ cozo:close(Db) || Db <- Dbs ].
