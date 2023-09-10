%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cozo_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("cozo.hrl").

% helper to create ok queries
-define(QUERY_OK(N,Q),
        begin
            (fun() ->
                     {ok, {N, R}} = cozo:open(),
                     {ok, E} = cozo:run(N, Q),
                     LogFormat = "db: ~p~n"
                         "query: ~s~n"
                         "result: ~p~n"
                         "state: ~p",
                     LogArgs = [N, Q, E, R],
                     ct:pal(info, ?LOW_IMPORTANCE, LogFormat, LogArgs),
                     ok = cozo:close(N)
             end)()
        end).

% helper to create error queries
-define(QUERY_ERROR(N,Q),
        begin
            (fun() ->
                     {ok, {N, R}} = cozo:open(),
                     {error, E} = cozo:run(N, Q),
                     LogFormat = "db: ~p~n"
                         "query: ~s~n"
                         "result: ~p~n"
                         "state: ~p~n",
                     LogArgs = [N, Q, E, R],
                     ct:pal(info, ?LOW_IMPORTANCE, LogFormat, LogArgs),
                     ok = cozo:close(N)
             end)()
        end).

-define(IQUERY_LOG(DB, QUERY),
        begin
            (fun() ->
                     {ok, R} = cozo:run(DB, QUERY),
                     LogFormat = "db: ~p~nquery: ~s~nresult: ~p",
                     LogArgs = [DB,QUERY,R],
                     ct:pal(info, ?LOW_IMPORTANCE, LogFormat, LogArgs)
             end)()
        end).

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

all() -> [ tutorial_intro, tutorial_expressions, tutorial_rules
         , tutorial_stored_relations, tutorial_command_blocks
         , tutorial_graphs, air_routes, simple, multi_spawn].

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#First-steps
%%--------------------------------------------------------------------
tutorial_intro() -> [].
tutorial_intro(_Config) ->
    ?QUERY_OK(Db0, "?[] <- [['hello', 'world', 'Cozo!']]"),
    ?QUERY_OK(Db1, "?[] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK(Db2, "?[] <- [[1.5, 2.5, 3, 4, 5.5],"
              "['aA', 'bB', 'cC', 'dD', 'eE'],"
              "[true, false, null, -1.4e-2, \"A string with double quotes\"]]"),
    ?QUERY_OK(Db3, "?[] <- [[1], [2], [1], [2], [1]]").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Expressions
%%--------------------------------------------------------------------
tutorial_expressions() -> [].
tutorial_expressions(_Config) ->
    ?QUERY_OK(Db4, "?[] <- [["
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
    ?QUERY_OK(Db5, "a[x, y] <- [[1, 2], [3, 4]]"
              "b[y, z] <- [[2, 3], [2, 4]]"
              "?[x, y, z] := a[x, y], b[y, z]"
              "?[x, y, z] := a[x, y], not b[y, _], z = null").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
%%--------------------------------------------------------------------
tutorial_rules() -> [].
tutorial_rules(_Config) ->
    ?QUERY_OK(Db6, "?[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK(Db7, "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[a, b, c] := rule[a, b, c]"),
    ?QUERY_OK(Db8, "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b] := rule[a, b, c]"),
    ?QUERY_OK(Db9, "?[c, b] := rule[a, b, c], is_num(a)"
              "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"),
    ?QUERY_OK(Db10, "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b] := rule['a', b, c]"),
    ?QUERY_OK(Db11, "rule[first, second, third] <- [[1, 2, 3], ['a', 'b', 'c']]"
              "?[c, b, d] := rule[a, b, c], is_num(a), d = a + b + 2*c"),
    ?QUERY_OK(Db12, "?[x, y] := x in [1, 2, 3], y in ['x', 'y']").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
%%--------------------------------------------------------------------
tutorial_joins() -> [].
tutorial_joins(_Config) ->
    ?QUERY_OK(Db13, "r1[] <- [[1, 'a'], [2, 'b']]"
              "r2[] <- [[2, 'B'], [3, 'C']]"
              "?[l1, l2] := r1[a, l1], r2[b, l2]"),

    ?QUERY_OK(Db14, "r1[] <- [[1, 'a'], [2, 'b']]"
              "r2[] <- [[2, 'B'], [3, 'C']]"
              "?[l1, l2] := r1[a, l1],"
              "             r2[a, l2]"),

    ?QUERY_OK(Db15, "a[x, y] <- [[1, 2], [3, 4]]"
              "b[y, z] <- [[2, 3], [2, 4]]"
              "?[x, y, z] := a[x, y], b[y, z]"
              "?[x, y, z] := a[x, y], not b[y, _], z = null").

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Stored-relations
%%--------------------------------------------------------------------
tutorial_stored_relations() -> [].
tutorial_stored_relations(_Config) ->
    {ok, {Db, _}} = cozo:open(),
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
    ok = cozo:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Command-blocks
%%--------------------------------------------------------------------
tutorial_command_blocks() -> [].
tutorial_command_blocks(_Config) ->
    {ok, {Db, _}} = cozo:open(),
    ?IQUERY_LOG(Db, "{?[a] <- [[1], [2], [3]]; :replace test {a}}"
                "{?[a] <- []; :replace test2 {a}}"
                "%swap test test2"
                "%return test"),
    ok = cozo:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Graphs
%%--------------------------------------------------------------------
tutorial_graphs() -> [].
tutorial_graphs(_Config) ->
    {ok, {Db, _}} = cozo:open(),
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
    ok = cozo:close(Db).

%%--------------------------------------------------------------------
%% https://docs.cozodb.org/en/latest/tutorial.html#Extended-example:-the-air-routes-dataset
%%--------------------------------------------------------------------
air_routes() -> [].
air_routes(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, RawAirRoutes} = file:read_file(filename:join(DataDir, "air-routes.json")),
    {ok, AirRoutes} = thoas:decode(RawAirRoutes),
    {ok, {Db, _}} = cozo:open(),
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

    cozo:import_backup(Db, AirRoutes),

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
    %% 		"?[code, desc, score] := centrality[code, score], *airport{code, desc}"
    %% 		":limit 10;"
    %% 		":order -score"),
    
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
  
    ok = cozo:close(Db).

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
simple() -> [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
simple(_Config) ->
    {ok, {Db, _}} = cozo:open(),
    {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),
    cozo:close(Db).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
sqlite() -> [].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
sqlite(_Config) ->
    % create a new cozo database using sqlite
    {ok, {Db, #cozo{ db_path = Path }}} = cozo:open(sqlite),
    true = filelib:is_file(Path),
    {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),    
    {ok, _} = cozo:create_relations(Db, "stored" "{c1, c2}"),
    {ok, _} = cozo:run(Db, ":create dept_info {"
	     "company_name: String,"
	     "department_name: String,"
	     "=>"
	     "head_count: Int default 0,"
	     "address: String,"
	     "}"),
    {ok, _} = cozo:run(Db, "?[a, b, c] <- [[1, 'a', 'A'],"
		       "[2, 'b', 'B'],"
		       "[3, 'c', 'C'],"
		       "[4, 'd', 'D']]"),
    cozo:close(Db),

    % Reopen the database
    {ok, {Db2, #cozo{ db_path = Path }}} = cozo:open(sqlite, Path),
    true = filelib:is_file(Path),
    {ok, _} = cozo:run(Db2, "?[] <- [[1, 2, 3]]"),
    {ok, _} = cozo:run(Db2, "?[a, b, c] <- [[1, 'a', 'A'],"
		       "[2, 'b', 'B'],"
		       "[3, 'c', 'C'],"
		       "[4, 'd', 'D']]"),
    cozo:close(Db2),
    
    % cleanup the file
    file:delete(Path).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
rocksdb() -> [].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
rocksdb(_Config) ->
    {ok, {Db, #cozo{ db_path = Path }}} = cozo:open(rocksdb),
    filelib:is_dir(Path),
    {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),    
    {ok, _} = cozo:create_relations(Db, "stored" "{c1, c2}"),
    {ok, _} = cozo:run(Db, ":create dept_info {"
	     "company_name: String,"
	     "department_name: String,"
	     "=>"
	     "head_count: Int default 0,"
	     "address: String,"
	     "}"),
    {ok, _} = cozo:run(Db, "?[a, b, c] <- [[1, 'a', 'A'],"
		       "[2, 'b', 'B'],"
		       "[3, 'c', 'C'],"
		       "[4, 'd', 'D']]"),
    cozo:close(Db),
    
    {ok, {Db2, #cozo{ db_path = Path }}} = cozo:open(rocksdb, Path),
    filelib:is_dir(Path),
    {ok, _} = cozo:run(Db2, "?[] <- [[1, 2, 3]]"),
    {ok, _} = cozo:run(Db2, "?[a, b, c] <- [[1, 'a', 'A'],"
		       "[2, 'b', 'B'],"
		       "[3, 'c', 'C'],"
		       "[4, 'd', 'D']]"),
    cozo:close(Db2).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
system_commands() -> [].
system_commands(_Config) ->
    {ok, {Db, _}} = cozo:open(),
    {ok, _} = cozo:create_relation(Db, "stored", "c1"),
    {ok, _} = cozo:create_relation(Db, "stored2", ["c1","c2","c3"]),
    {ok, _} = cozo:create_relation(Db, stored3, c1),
    {ok, _} = cozo:create_relation(Db, stored4, [c1,c2,c3]),
    {ok, _} = cozo:create_relation(Db, stored5, ["c1",c2,"c3",c4]),
    ok = cozo:close(Db).

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
multi_spawn() -> [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
multi_spawn(_Config) ->
    [ cozo_spawn(100)
    , cozo_spawn(1_000)
    , cozo_spawn(10_000)
    % , cozo_spawn(100_000)
    ].

cozo_spawn(Counter) ->
  Open = fun() -> {ok, {Db, _R}} = cozo:open(), Db end,
  Dbs = [ Open() || _ <- lists:seq(1, Counter) ],
  Run = fun(Db) -> spawn(cozo_nif, run, [Db, "?[] <- [[1, 2, 3]]"]) end,
  [ Run(Db) || Db <- Dbs ],
  [ cozo:close(Db) || Db <- Dbs ].
