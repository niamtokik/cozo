---
status: draft
date: 2023-09-13
title: Integrating CozoDB in Erlang Ecosystem
subtitle: Mixing NIF Interfaces, CozoDB, Datalog and Erlang Together
author: Mathieu Kerjouan 
keywords: [cozo,cozodb,erlang,otp,datalog]
license: CC BY-NC-ND
abstract: |
  Erlang/OTP has been created with an incredible toolbox, 
  including different application to store data. ETS, an in-memory 
  Erlang term storage used as cache. DETS, an long term on disk 
  storage facility based on ETS. Finally, Mnesia, a database using
  ETS and DETS to create distributed DBMS. These applications are
  not external projects but are delivered by default with each
  releases.
---

## Introduction

 - [ ] introducing Datalog
 - [ ] introducing Cozodb
 - [ ] introducing NIF

> Datalog is a declarative logic programming language. While it is
> syntactically a subset of Prolog, Datalog generally uses a bottom-up
> rather than top-down evaluation model. This difference yields
> significantly different behavior and properties from Prolog. It is
> often used as a query language for deductive databases. Datalog has
> been applied to problems in data integration, networking, program
> analysis, and more.[^datalog-wikipedia]

Datalog is greatly inspired from Prolog, and because many Datalog
implementation are not free and open-source, I will try to explain the
concept behind Datalog using Prolog. When programming with Logic
Programming languages like Prolog, a knowledge base is accessible
during the execution of the program. You can see it as a database
containing terms for the moment. If you can add terms, you can also
design them. They you want to store users in your knowledge based. An
user is defined by its name, its age and its password. An entry is
called a fact, and we can add new one using `assert/1` followed by the
definition of a new fact. Let adds three new user, *John Smith*, *Bill
Kill* and "Luke Skywalker*.

```prolog
assert(user("John Smith", 42, "StrongPassword")).
assert(user("Bill Kill", 57, "Beatrix")).
assert(user("Luke Skywalker", 24, "IHateBranda")).
```

Directly from the REPL, you can easily extract the password by
creating a query using `user/3` predicate composed by the name of the
user as a static value, an empty variable and a named variable called
`Password`. This last will return the content of the password field
from the user.

```prolog
user("John Smith", _, Password).
% Password = "Strong Password"
```

More than one facts have been added into the knowledge base, and using
`findall/3` predicate, we can easily extract all the user name from
it.

```prolog
findall(Name, user(Name, _, _), Xs).
% Xs = ["John Smith", "Bill Kill", "Luke Skywalker"].
```

Filtering using guards can also be done.

```prolog
findall(Name, (user(Name, Age, _), Age>40), Xs).
% Xs = ["John Smith", "Bill Kill"].
```

Creating more complex data-structures by composing data from facts is
also possible.

```prolog
findall({Name, Password}, (user(Name, Age, Password)), Xs).
% Xs = [{"John Smith", "Strong Password"}
%     ,{"Bill Kill", "Beatrix"}
%     ,{"Luke Skywalker", "IHateBranda"}].
```

Finally, if you want to extract all facts from the database, it's also
an easy task.

```prolog
findall(user(Name, Age, Password), (user(Name, Age, Password)), Xs).
% Xs = [user("John Smith", 42, "Strong Password")
%      ,user("Bill Kill", 57, "Beatrix")
%      , user("Luke Skywalker", 24, "IHateBranda")].
```

[^datalog-wikipedia]:(https://en.wikipedia.org/wiki/Datalog)

## A Quick Overview of CozoDB

 - [ ] using CozoDB REPL
 - [ ] describe and use Cozoscript syntax

> A FOSS embeddable, transactional, relational-graph-vector database,
> across platforms and languages, with time travelling capability,
> perfect as the long-term memory for LLMs and
> AI.[^cozo-official-website]

[^cozo-official-website]: https://www.cozodb.org/

## CozoDB Interface with `libcozo_c`

 - [ ] build `cozo` project and `libcozo_c` library
 - [ ] describe `libcozo_c` interfaces
 - [ ] describe `Makefile` and make command facility

`libcozo_c` is a compatible C interface to CozoDB. A release is
available on the official CozoDB Github repository but it can be
manually compiled using `cargo`.

```sh
git clone https://github.com/cozodb/cozo
cd cozo
cargo build -p release cozo
```

A `Makefile` has been created at the root of Erlang `cozo` project to
fetch the right version currently supported.

```sh
make deps
```

`ei.h` C header is required.

```c
#include <ei.h>
```

```c
#include <erl_nif.h>
```

```c
#include "cozo_c.h"
```

Because this implementation is using an external library, the
definition of each exported functions from this one needs to be
created prefixed with the keyword `extern` meaning "external
references" outside of the scope of this program. It's basically a
copy/paste from `cozo_c.h ` prototypes.

```c
extern void cozo_free_str(char *s);
extern char *cozo_open_db(const char *engine, const char *path, 
                          const char *options, int32_t *db_id);
extern bool cozo_close_db(int32_t id);
extern char *cozo_run_query(int32_t db_id, const char *script_raw, 
                            const char *params_raw, bool immutable_query);
extern char *cozo_import_relations(int32_t db_id, const char *json_payload);
extern char *cozo_export_relations(int32_t db_id, const char *json_payload);
extern char *cozo_backup(int32_t db_id, const char *out_path);
extern char *cozo_restore(int32_t db_id, const char *in_path);
extern char *cozo_import_from_backup(int32_t db_id, const char *json_payload);
```

`ERL_NIF_INIT` macro is needed to generate other internal functions
and configure the NIF correctly. The first argument is the name of the
module called `cozo_nif` here, and ...

```c
ERL_NIF_INIT(cozo_nif,nif_funcs,NULL,NULL,NULL,NULL)
```

The list of functions and their mapping on the Erlang side is defined
in `ErlNifFunc`. 8 functions are exported.

```c
static ErlNifFunc nif_funcs[] =
  {
   {"open_db_nif", 3, open_db},
   {"close_db_nif", 1, close_db},
   {"run_query_nif", 4, run_query},
   {"import_relations_db_nif", 2, import_relations_db},
   {"export_relations_db_nif", 2, export_relations_db},
   {"backup_db_nif", 2, backup_db},
   {"restore_db_nif", 2, restore_db},
   {"import_backup_db_nif", 2, import_backup_db}
  };
```

```c
ERL_NIF_TERM atom_ok(ErlNifEnv *env) {
  const char* atom = "ok";
  return enif_make_atom(env, atom);
}
```

```c
ERL_NIF_TERM atom_error(ErlNifEnv *env) {
  const char* atom = "error";
  return enif_make_atom(env, atom);
}
```

## Erlang Interface to NIF

 - [ ] describe `cozo_nif` module
 - [ ] describe `cozo` module
 - [ ] describe `cozo_db` module

```erlang
{ok, DbId} = cozo_nif:new().
ok = cozo_nif:close(DbId).
```

```erlang
{ok, DbId2} = cozo_nif:new("mem\n").
ok = cozo_nif:close(DbId2).
```

```erlang
{ok, DbId3} = cozo_nif:new("mem\n", "my_path\n").
ok = cozo_nif:close(DbId3).
```

```erlang
{ok, DbId4} = cozo_nif_new("mem\n", "my_path\n", "{}\n").
ok = cozo_nif:close(DbId4).
```

## Using CozoDB with Erlang

 - [ ] usage example with `cozo` module
 - [ ] usage example with `cozo_db` module

Types and data-structures are defined in 

```erlang
-type db_id()         :: 0 | pos_integer().
-type db_engine()     :: mem | sqlite | rocksdb.
-type db_path()       :: string().
-type db_options()    :: map().
-type db_parent()     :: pid().
-type db_query()      :: string() | binary() | [string(), ...].
```

```erlang
-record(cozo, { id = undefined        :: undefined | db_id()
              , db_engine = mem       :: db_engine()
              , db_path = ""          :: db_path()
              , db_options = #{}      :: db_options()
              , db_parent = undefined :: undefined | db_parent()
              }).
-type cozo() :: #cozo{ db_parent :: undefined | db_parent() }.
```

```erlang
{ok, Db} = cozo:new().
```

```erlang
0 = cozo:get_id(Db).
```

```erlang
ok = cozo:close(Db).
```

`cozo_db` was created to isolated a database behind an Erlang process
and linearize the queries and answers. The idea is to offer an easy
way to synchronize and distribute queries in a cluster
environment. All interfaces are similar than the ones defined in
`cozo` module.

```erlang
{ok, Db} = cozo_db:new().
ok = cozo_db:close(Db).

{ok, DbMonitored} = cozo_db:start_monitor().
ok = cozo_db:stop(DbMonitored).

{ok, DbLinked} = cozo_db:start_link().
ok = cozo_db:stop(DbLinked).
```

CozoDB team created a pretty nice tutorial, with many use case. This
was quite useful to test the implementation and have an idea if
everything was working correctly. This tutorial used with `cozo` and
`cozo_db` modules were created using
`common_test`[^erlang-common_test].

[^erlang-common_test]: [https://www.erlang.org/doc/man/common_test.html#](https://www.erlang.org/doc/man/common_test.html)

## Future Improvements

Using Cozoscript to execute queries on CozoDB is enough if you want to
create a simple interface to users wanting their own isolated
environment using something already documented and
tested. Unfortunately, this feature is hard to integrate *correctly*
with Erlang, and by *correcty* I mean creating a request and get back
a result with Erlang terms. In fact, creates something similar to
Match Specifications[^erlang-match-specification] in Erlang.

To accomplish something like this, a new interface using Rust[^rust]
and Rustler[^rustler] with `cozo-core`[^cozo-core] should be used
instead of `libcozo_c`. It will require an important amount of work
but should offer a better integration with Erlang, by letting this
langage generating CozoDB bytecode.

Another improvement, perhaps before doing bytecode compilation from
Erlang to CozoDB, is to create a fully compatible Cozoscript
implementation in pure Erlang. Indeed, the
specifications[^cozoscript-specifications] are freely available and
can be easily converted to something Erlang compatible with
`leex`[^erlang-leex] and `yecc`[^erlang-yecc].

Finally, `erlang_nif.c` is not tested against memory leaks and other
kind of C related issues. It should be planned to create test suites
and analyze this part of the code with Valgrind[^valgrind].

[^erlang-match-specification]: [https://www.erlang.org/doc/apps/erts/match_spec.html](https://www.erlang.org/doc/apps/erts/match_spec.html)
[^rust]: [https://www.rust-lang.org/](https://www.rust-lang.org/)
[^rustler]: [https://docs.rs/crate/rustler](https://docs.rs/crate/rustler)
[^cozo-core]: [https://github.com/cozodb/cozo/tree/main/cozo-core](https://github.com/cozodb/cozo/tree/main/cozo-core)
[^cozoscript-specifications]: [https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest](https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest)
[^erlang-leex]: [https://www.erlang.org/doc/man/leex](https://www.erlang.org/doc/man/leex)
[^erlang-yecc]: [https://www.erlang.org/doc/man/yecc](https://www.erlang.org/doc/man/yecc)
[^valgrind]: [https://valgrind.org/](https://valgrind.org/)

## Conclusion

Integrating `libcozo_c` using Erlang NIF was easier than
anticipated. In fact, CozoDB project is well documented. The library
was nicely designed, and the interfaces were quickly created without
big trouble. Thus, one the most annoying problem was related to local
path for C libraries and headers, but this is related to LLVM or GCC
and the configuration used during this implementation. In less than a
week, this application was usable, documented, tested and
specified.

Special thanks to Alejandro M. Ramallo[^alejandro-m-ramallo-github]
who shared this project idea with me and was here to add MacOS
support.

[^alejandro-m-ramallo-github]: [https://github.com/aramallo](https://github.com/aramallo)

## References and Resources

*Erlang Cozo Project on Github*,
[https://github.com/niamtokik/cozo](https://github.com/niamtokik/cozo)
 
*CozoDB Official Website*,
[https://www.cozodb.org/](https://www.cozodb.org/)
 
*CozoDB Official Documentation*,
[https://docs.cozodb.org/en/latest/](https://docs.cozodb.org/en/latest/)
 
*CozoDB Official Repository*,
[https://github.com/cozodb/cozo](https://github.com/cozodb/cozo)
 
*CozoDB 0.7.2 Release*,
[https://github.com/cozodb/cozo/releases/tag/v0.7.2](https://github.com/cozodb/cozo/releases/tag/v0.7.2)
 
*Erlang NIFs Tutorial*,
[https://www.erlang.org/doc/tutorial/nif.html](https://www.erlang.org/doc/tutorial/nif.html)
 
*Erlang `erl_nif` Manual Page*
[https://www.erlang.org/doc/man/erl_nif.html](https://www.erlang.org/doc/man/erl_nif.html)
 
*Erlang `ets` Reference Manual*,
[https://www.erlang.org/doc/man/ets.html](https://www.erlang.org/doc/man/ets.html)
 
*Erlang `dets` Reference Manual*,
[https://www.erlang.org/doc/man/dets.html](https://www.erlang.org/doc/man/dets.html)
 
*Erlang `mnesia` Reference Manual*,
[https://www.erlang.org/doc/apps/mnesia/index.html](https://www.erlang.org/doc/apps/mnesia/index.html)
