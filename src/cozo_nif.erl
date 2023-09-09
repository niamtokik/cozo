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
%%%
%%% At this time, this module convert a string (list of integers) to C
%%% string. All strings MUST be terminated by NULL (`0x00') or by
%%% carriage return (`\n'). If you are using this interface, you are
%%% in charge of adding one of these characters by yourself.
%%%
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

%---------------------------------------------------------------------
% local type definition
%---------------------------------------------------------------------
-type db_engine() :: string().
-type db_path() :: string().
-type db_options() :: string().
-type db_id() :: 0 | pos_integer().
-type query_script() :: string().
-type query_params() :: string().
-type query_mutable() :: 0 | 1.
-type json() :: string().
-type nif_return_ok() :: {ok, json()}.
-type nif_return_error() :: {error, json()}.
-type nif_return() :: nif_return_ok()
        | nif_return_error().

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.

init() -> init("cozo_nif").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Path) -> Return when
      Path   :: db_path(),
      Return :: ok.

init(Path) ->
    PrivDir = application:get_env(cozo, lib_path, priv_dir()),
    Lib = filename:join(PrivDir, Path),
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Path]}]),
    ok = erlang:load_nif(Lib, 0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc open_db nif function from cozo_nif.c
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L35
%%
%% == Examples ==
%%
%% ```
%% {ok, DbId} = cozo_nif:open("mem\n", "/tmp/cozodb.db\n", "{}\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec open_db(Engine, Path, Options) -> Return when
      Engine  :: db_engine(),
      Path    :: db_path(),
      Options :: db_options(),
      Return  :: nif_return().

open_db(_Engine, _Path, _Options) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc run_query nif function from cozo_nif.c
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L62
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:run_query("?[] <- [[1,2,3]]\n", "{}\n", 0).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec run_query(Id, Script, Params, Mutable) -> Return when
      Id      :: db_id(),
      Script  :: query_script(),
      Params  :: query_params(),
      Mutable :: query_mutable(),
      Return  :: nif_return().

run_query(_Id, _Script, _Params, _Mutable) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Close an opened database.
%%
%% close_db nif function from cozo_nif.c
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L45
%%
%% == Examples ==
%%
%% ```
%% ok = cozo_nif:close(DbId).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec close_db(Id) -> Return when
      Id     :: db_id(),
      Return :: nif_return().

close_db(_Id) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Import json data into relations.
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L78
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:import_relations_db(DbId, "{}\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec import_relations_db(Id, Json) -> Return when
      Id     :: db_id(),
      Json   :: json(),
      Return :: nif_return().

import_relations_db(_Id, _Json) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Export relations as json object.
%%
%% export_relations nif function from cozo_nif.c.
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L89
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:export_relations_db(DbId, "{}\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec export_relations_db(Id, Json) -> Return when
      Id     :: db_id(),
      Json   :: json(),
      Return :: nif_return().

export_relations_db(_Id, _Json) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Backup database to a path.
%%
%% backup nif function from cozo_nif.c.
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L100
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:backup_db(DbId, "/tmp/backup.db\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec backup_db(Id, Path) -> Return when
      Id     :: db_id(),
      Path   :: db_path(),
      Return :: nif_return().

backup_db(_Id, _Path) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Restore db from a path.
%%
%% restore nif function fro cozo_nif.c.
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L111
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:restore_db(DbId, "/tmp/backup.db\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec restore_db(Id, Path) -> Return when
      Id     :: db_id(),
      Path   :: db_path(),
      Return :: nif_return().

restore_db(_Id, _Path) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @doc Import db from backup file.
%%
%% see https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L125
%%
%% == Examples ==
%%
%% ```
%% {ok, Result} = cozo_nif:import_backup_db(DbId, "/tmp/backup.db\n").
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec import_backup_db(Id, Path) -> Return when
      Id     :: db_id(),
      Path   :: db_path(),
      Return :: nif_return().

import_backup_db(_Id, _Path) ->
    exit(nif_library_not_loaded).

%%--------------------------------------------------------------------
%% @hidden
%% @private
%% @doc Returns the app's priv dir.
%% @end
%%--------------------------------------------------------------------
-spec priv_dir() -> string().

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
