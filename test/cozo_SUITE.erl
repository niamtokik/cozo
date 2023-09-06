%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(cozo_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

% helper to create ok queries
-define(QUERY_OK(N,Q), {ok, N} = cozo:open(),
                       {ok, _} = cozo:run(N, Q),
                       ok = cozo:close(N)).

% helper to create error queries
-define(QUERY_ERROR(N,Q), {ok, N} = cozo:open(),
	                  {error, _} = cozo:run(N, Q),
                          ok = cozo:close(N)).


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

all() -> [tutorial_intro, tutorial_expressions, tutorial_rules
         ,tutorial_stored_relations, simple, multi_spawn].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
tutorial_intro() -> [].
tutorial_intro(_Config) ->
  % https://docs.cozodb.org/en/latest/tutorial.html#First-steps
  ?QUERY_OK(Db0, "?[] <- [['hello', 'world', 'Cozo!']]"),

  ?QUERY_OK(Db1, "?[] <- [[1, 2, 3], ['a', 'b', 'c']]"),

  ?QUERY_OK(Db2, "?[] <- [[1.5, 2.5, 3, 4, 5.5],"
	    "['aA', 'bB', 'cC', 'dD', 'eE'],"
	    "[true, false, null, -1.4e-2, \"A string with double quotes\"]]"),

  ?QUERY_OK(Db3, "?[] <- [[1], [2], [1], [2], [1]]").

tutorial_expressions() -> [].
tutorial_expressions(_Config) ->
  % https://docs.cozodb.org/en/latest/tutorial.html#Expressions
  % this one crash
  % ?QUERY_ERROR(Db4, "?[] <- [["
  %	       "1 + 2, # addition"
  %	       "3 / 4, # division"
  %	       "5 == 6, # equality"
  %	       "7 > 8, # greater"
  %	       "true || false, # or"
  %	       "false && true, # and"
  %	       "lowercase('HELLO'), # function"
  %	       "rand_float(), # function taking no argument"
  %	       "union([1, 2, 3], [3, 4, 5], [5, 6, 7]), # variadic function"
  %	       "]]"),

  ?QUERY_OK(Db5, "a[x, y] <- [[1, 2], [3, 4]]"
	    "b[y, z] <- [[2, 3], [2, 4]]"
	    "?[x, y, z] := a[x, y], b[y, z]"
	    "?[x, y, z] := a[x, y], not b[y, _], z = null").

tutorial_rules() -> [].
tutorial_rules(_Config) ->
  % https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
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

tutorial_joins() -> [].
tutorial_joins(_Config) ->
  % https://docs.cozodb.org/en/latest/tutorial.html#Joins,-made-easy
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

tutorial_stored_relations() -> [].
tutorial_stored_relations(_Config) ->
  % https://docs.cozodb.org/en/latest/tutorial.html#Stored-relations
  % this one crash
  ?QUERY_ERROR(Db16, ":create stored {c1, c2}").

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
    {ok, Db} = cozo:open(),
    {ok, _} = cozo:run(Db, "?[] <- [[1, 2, 3]]"),
    cozo:close(Db).

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
  Limit = 100000,
  Open = fun() -> {ok, Db} = cozo:open(), Db end,
  Dbs = [ Open() || _ <- lists:seq(1,Limit) ],
  Run = fun(Db) -> spawn(cozo_nif, run, [Db, "?[] <- [[1, 2, 3]]"]) end,
  [ Run(Db) || Db <- Dbs ],
  [ cozo:close(Db) || Db <- Dbs ].
