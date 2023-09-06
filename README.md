# cozo

A quick and dirty NIF interface to cozodb.

## Todo

This is just a PoC... Lot of things to do.

 - [ ] create test suite
 - [ ] and much more...

## Support

 - [x] `cozo_open_db` with `cozo:open/0` and `cozo:open/2`.
 - [x] `cozo_close_db` with `cozo:close/1`
 - [x] `cozo_run_query` with `cozo:run/2`, `cozo:run/3` and `cozo:run/4`
 - [x] `cozo_import_relations` with `cozo:import_relations/2`
 - [x] `cozo_export_relations` with `cozo:export_relations/2`
 - [x] `cozo_backup`  with `cozo:backup/2`
 - [x] `cozo_restore`  with `cozo:restore/2`
 - [x] `cozo_import_from_backup`  with `cozo:import_backup/2`

## Build

```sh
make all

# Issue with libraries and path... Just use that to fix
# temporarily the issue
# you can check with 
#    $ LD_PRELOAD_PATH=$(pwd)/c_src ldd c_src/libcozo_c.so
export LD_PRELOAD_PATH=$(pwd)/c_src

# If it still does not work, copy libcozo_c.so in /usr/lib
# This is not the best method though, but it will give you
# access to it.
sudo cp c_src/libcozo_c.so /usr/lib
```

```sh
rebar3 compile
```

## Test

```sh
make clean all test
```

## Usage

```erlang
% open a new database in memory 
{ok,0} = cozo:open().

% run a query
3> 
{ok,#{ <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>],
       <<"next">> => null,
       <<"ok">> => true,
       <<"rows">> => [[1,2,3]],
       <<"took">> => 0.001401927
     }
} = cozo:run(Db, "?[] <- [[1, 2, 3]]").

% close the database
ok = cozo:close(Db).
```
