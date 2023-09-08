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
%%% @doc
%%%
%%% In memory CozoDB behind TCP server. A quick and dirty way to play
%%% with cozodb with only netcat.
%%%
%%% ```
%%% cozo_tcp:start().
%%% '''
%%%
%%% ```
%%% $ nc localhost 6543
%%% ?[] <- [[1,2,3]]
%%% {"headers":["_0","_1","_2"],"next":null,"ok":true,"rows":[[1,2,3]],"took":0.007618698}
%%% '''
%%%
%%% @end
%%%===================================================================
-module(cozo_tcp).
-compile(export_all).
-behavior(gen_server).
-export([start/1, start_link/1]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start() -> start([]).
start(Args) -> gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() -> [].
start_link(Args) -> gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stop() -> gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init(Args) ->
    erlang:process_flag(trap_exit, true),
    Port = proplists:get_value(port, Args, 6543),
    Opts = proplists:get_value(opts, Args, [{active, true}]),
    {ok, Socket} = gen_tcp:listen(Port, Opts),
    [ acceptor_start(Socket) || _ <- lists:seq(1,1000) ],
    {ok, Socket}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(Socket) ->
    ProcessList = erlang:process_info(self(), [links]),
    Links = proplists:get_value(links, ProcessList, []),
    [ erlang:exit(Pid, normal) || Pid <- Links ],
    gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call(_,_,Socket) ->
    {reply, ok, Socket}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(_,Socket) ->
    {noreply, Socket}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info({'DOWN', _, process, _, _}, Socket) ->
    acceptor_start(Socket),
    {noreply, Socket};
handle_info({'EXIT', _, _}, Socket) ->
    acceptor_start(Socket),
    {noreply, Socket}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
acceptor_start(Socket) ->
    spawn_link(fun() -> acceptor_init(Socket) end).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
acceptor_init(Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, Port} -> acceptor_db(Port);
	{error, closed} -> ok
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
acceptor_db(Socket) ->
    case cozo:open() of
	{ok, {Db, _}} ->
	    acceptor_loop(Db, Socket);
	_ ->
	    gen_tcp:close(Socket)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
acceptor_loop(Db, Socket) ->
    receive
	{tcp, Socket, Message} ->
	    acceptor_exec(Message, Db, Socket);
	{tcp_closed, Socket} ->
	    gen_tcp:close(Socket);
	Elsewise ->
	    io:format("received: ~p~n", [Elsewise]),
	    acceptor_loop(Db, Socket)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
acceptor_exec(Message, Db, Socket) ->
    try
	case cozo:run(Db, Message) of
	    {ok, Result} ->
		Encoded = thoas:encode(Result),
		gen_tcp:send(Socket, binary_to_list(Encoded)),
		acceptor_loop(Db, Socket);
	    {error, _Reason} ->
		gen_tcp:send(Socket, "error"),
		acceptor_loop(Db, Socket)
	end
    catch
	_:_ -> acceptor_loop(Db, Socket)
    end.
