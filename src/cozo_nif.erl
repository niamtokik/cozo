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
%%% @doc Raw NIF implementation of cozodb. The library must be
%%% compiled first.
%%% @end
%%%===================================================================
-module(cozo_nif).
-export([ open_db/3, close_db/1, run_query/4
	, import_relations_db/2, export_relations_db/2
	, backup_db/2, restore_db/2, import_backup_db/2
	]).
-nifs([ open_db/3, close_db/1, run_query/4
      , import_relations_db/2, export_relations_db/2
      , backup_db/2, restore_db/2, import_backup_db/2
      ]).
-include_lib("kernel/include/logger.hrl").
-on_load(init/0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init() -> init("cozo_nif").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
init(Path) ->
    PrivDir = application:get_env(cozo, lib_path, priv_dir()),
    Lib = filename:join(PrivDir, Path),
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Path]}]),
    ok = erlang:load_nif(Lib, 0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc open_db nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L35
%% @end
%%--------------------------------------------------------------------
open_db(_Engine, _Path, _Options) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc run_query nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L62
%% @end
%%--------------------------------------------------------------------
run_query(_Id, _Script, _Params, _Mutable) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc close_db nif function from cozo_nif.c
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L45
%% @end
%%--------------------------------------------------------------------
close_db(_Id) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
import_relations_db(_Id, _Json) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
export_relations_db(_Id, _Json) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
backup_db(_Id, _Path) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
restore_db(_Id, _Path) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
import_backup_db(_Id, _Path) ->
    exit(nif_library_not_loaded).


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Returns the app's priv dir
%% @end
%% -----------------------------------------------------------------------------
priv_dir() ->
    case code:priv_dir(cozo) of
	{error, bad_name} ->
	    case code:which(?MODULE) of
		FN when is_list(FN) ->
		  filename:join([filename:dirname(FN), "..", "priv"]);
		_ ->
		  "../priv"
	    end;
	Val ->
	    Val
    end.
