%%%-------------------------------------------------------------------
%% @doc cozo public API
%% @end
%%%-------------------------------------------------------------------

-module(cozo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cozo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
