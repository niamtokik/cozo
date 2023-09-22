# cozo

![Cozo License](https://img.shields.io/github/license/niamtokik/cozo)
![Cozo Top Language](https://img.shields.io/github/languages/top/niamtokik/cozo)
![Cozo Workflow Status (main branch)](https://img.shields.io/github/actions/workflow/status/niamtokik/cozo/test.yaml?branch=main)
![Cozo Last Commit](https://img.shields.io/github/last-commit/niamtokik/cozo)
![Cozo Code Size (bytes)](https://img.shields.io/github/languages/code-size/niamtokik/cozo)
![Cozo Repository File Count](https://img.shields.io/github/directory-file-count/niamtokik/cozo)
![Cozo Repository Size](https://img.shields.io/github/repo-size/niamtokik/cozo)

An Erlang NIF wrapper for [CozoDB](https://www.cozodb.org), a FOSS
embeddable, transactional, relational-graph-vector database, with time
travelling capability, perfect as the long-term memory for LLMs and
AI.


## Support

 - [x] `cozodb` 0.7.5 on Linux and MacOS
 - [x] `cozo_open_db` with `cozo:open/0` and `cozo:open/2`.
 - [x] `cozo_close_db` with `cozo:close/1`
 - [x] `cozo_run_query` with `cozo:run/2`, `cozo:run/3` and `cozo:run/4`
 - [x] `cozo_import_relations` with `cozo:import_relations/2`
 - [x] `cozo_export_relations` with `cozo:export_relations/2`
 - [x] `cozo_backup`  with `cozo:backup/2`
 - [x] `cozo_restore`  with `cozo:restore/2`
 - [x] `cozo_import_from_backup`  with `cozo:import_backup/2`

## Todo

 - [x] Create test suite for standard commands (`cozo` module)
   - [x] test `cozo:open/0`, `cozo:open/1`, `cozo:open/2` and
         `cozo:open/3` functions
   - [x] test `cozo:run/2`, `cozo:run/3` and `cozo:run/4` functions
   - [x] test `cozo:close/1` function
   
 - [ ] Create test suite for maintenance commands (`cozo` module)
   - [x] test `cozo:import_relations/2` function
   - [x] test `cozo:export_relations/2` function
   - [x] test `cozo:backup/2` function
   - [ ] test `cozo:restore/2` function
   - [ ] test `cozo:import_backup/2` function
   
 - [ ] Create interfaces, documentation and test suites for system
       commands (`cozo` module)
   - [ ] `cozo:list_relations/1`
   - [ ] `cozo:remove_relation/2` and `cozo:remove_relations/2`
   - [ ] `cozo:create_relation/3`
   - [ ] `cozo:replace_relation/3`
   - [ ] `cozo:put_row/3`, `cozo:update_row/3`, `cozo:remove_row/3`
   - [ ] `cozo:ensure_row/3` and  `cozo:ensure_not_row/3`
   - [ ] `cozo:list_columns/2`
   - [ ] `cozo:list_indices/2`
   - [ ] `cozo:explain/2`
   - [ ] `cozo:describe/3`
   - [ ] `cozo:get_triggers/2`, `cozo:set_triggers/3`, `cozo:delete_triggers/2`
   - [ ] `cozo:set_access_level/3` 
   - [ ] `cozo:set_access_levels/3`
   - [ ] `cozo:get_running_queries/1`
   - [ ] `cozo:kill/2`
   - [ ] `cozo:compact/1`
 
 - [x] Create tests suite for different engines
   - [x] test `mem` engine
   - [x] test `sqlite` engine
   - [x] test `rocksdb` engine

 - [x] Create test suite for `cozo_nif` module

 - [x] Create `cozo_db` module to deal with strong isolation
 - [x] Specify interfaces
 - [ ] Add property based testing support
 - [x] Add Dialyzer support
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

Generate the project documentation.

```sh
make doc
```

Open the documentation.

```sh
open doc/index.html
```

Notes are also available in `notes` directory. You can exported them
in PDF, EPUB or HTML format if you have `pandoc` installed.

```sh
make notes
```

## Usage

Open a shell with `make`

```sh
make shell
```

If you want to create a totally isolated database in its own process,
you can use `cozo_db` module.

```erlang
% open a new database in memory
{ok, Pid} = cozo_db:start([]).

% run a query
{ok,#{ <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>],
       <<"next">> => null,
       <<"ok">> => true,
       <<"rows">> => [[1,2,3]],
       <<"took">> => 0.001401927
     }
} = cozo_db:run(Pid, "?[] <- [[1, 2, 3]]").

% close the database
ok = cozo_db:stop(Pid).
```

If you want to create more than one process and you don't care about
isolation, you can use `cozo` module.

```erlang
% open a new database in memory
{ok, Db} = cozo:open().

% run a query
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

If you want an access to a low level interface, you can also use
`cozo_nif` module.

## Examples

Some examples are present in `examples` directory like a cozo over
tcp, you can use with telnet or netcat.

```erlang
c("examples/cozo_tcp.erl").
{ok, Pid} = cozo_tcp:start().
```

```shell
nc localhost 6543
# ?[] <- [[1,2,3]]
```

## References

 - [Official CozoDB Website](https://www.cozodb.org/)
 - [Official CozoDB Documentation](https://docs.cozodb.org/en/latest/)
 - [Official CozoDB Repository](https://github.com/cozodb/cozo)
 - [Erlang NIF](https://www.erlang.org/doc/tutorial/nif.html)
