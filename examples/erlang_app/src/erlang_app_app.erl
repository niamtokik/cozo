%%%-------------------------------------------------------------------
%% @doc erlang_app public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
