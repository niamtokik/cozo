# cozo

A quick and dirty NIF interface to cozodb.

## Support

 - [x] `cozo_open_db` with `cozo:open/0` and `cozo:open/2`.
 - [x] `cozo_close_db` with `cozo:close/1`
 - [x] `cozo_run_query` with `cozo:run/2`, `cozo:run/3` and `cozo:run/4`
 - [x] `cozo_import_relations` with `cozo:import_relations/2`
 - [x] `cozo_export_relations` with `cozo:export_relations/2`
 - [x] `cozo_backup`  with `cozo:backup/2`
 - [x] `cozo_restore`  with `cozo:restore/2`
 - [x] `cozo_import_from_backup`  with `cozo:import_backup/2`

## Todo

 - [ ] Create test suite for `cozo` module
   - [x] test `cozo:open` function
   - [x] test `cozo:close` function
   - [x] test `cozo:run` function
   - [ ] test `cozo:import_relations` function
   - [ ] test `cozo:export_relation` function
   - [ ] test `cozo:backup` function
   - [ ] test `cozo:restore` function
   - [ ] test `cozo:import_backup` function
 - [ ] Create test sutie for `cozo_nif` module
 - [ ] Specify interfaces
 - [ ] Add property based testing support
 - [ ] Add Dialyzer support
 - [ ] Create more usage example
 - [ ] Create distributed example
 - [ ] Check if `cozo_nif.c` is safe

## Build

This project is using `Makefile` to extend the capability of
rebar3. everything can be easily done with it.

```sh
make all
# or
make deps compile
```

## Test

A full test suite is present in `test/cozo_SUITE.erl` file, using the
cozodb tutorial present in the official documentation as template.

```sh
make test
```

## Documentation

A short documentation

```sh
make doc
```

## Usage

Open a shell with `make`

```sh
make shell
```

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
