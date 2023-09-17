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
  including different application to store data. `ETS`, an in-memory
  Erlang term storage used as cache; `DETS`, an long term on disk
  storage facility based on ETS and finally, `Mnesia`, a database using
  `ETS` and `DETS` to create distributed DBMS. These applications are
  not external projects but are delivered by default with each
  releases. Erlang community is also providing some alternative, in
  particular with RocksDB. In this paper, an integration of CozoDB
  v0.7.2 is presented using its C interface called `libcozo_c`.
---

## Introduction

> Prolog is a logic programming language associated with artificial
> intelligence and computational linguistics. Prolog has its roots in
> first-order logic, a formal logic, and unlike many other programming
> languages, Prolog is intended primarily as a declarative programming
> language: the program logic is expressed in terms of relations,
> represented as facts and rules. A computation is initiated by
> running a query over these relations.[^prolog-wikipedia]

Datalog is greatly inspired from Prolog, and because many Datalog
implementation are not free and open-source, I will try to explain the
concept behind Datalog using Prolog. When programming with Logic
Programming languages like Prolog, a knowledge base is accessible
during the execution of the program. You can see it as a database
containing terms for the moment. If you can add terms, you can also
design them. They you want to store users in your knowledge based. An
user is defined by its name, its age and its password. An entry is
called a fact, and we can add new one using
`assert/1`[^prolog-assert/1] followed by the definition of a new
fact. Let adds three new user, *John Smith*, *Bill Kill* and *Luke
Skywalker*.

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
`findall/3`[^prolog-findall/3] predicate, we can easily extract all
the user name from it.

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
%      ,user("Luke Skywalker", 24, "IHateBranda")].
```

How to deal with information coming from another *table* and join
them? Let create a new collection of elements, outside of the scope of
the user predicate.

```prolog
assert(character("John Smith", male, bad)).
assert(character("Bill Kill", male, bad)).
assert(character("Luke Skywalker", male, good)).
```

We can easily merge both table together using `findall/3`.

```prolog
findall( {Name, Age, Sex, Type}
       , (user(Name, Age, _Password), character(Name, Sex, Type))
       , Xs).
% Xs = [{"John Smith", 42, male, bad}
%      ,{"Bill Kill", 57, male, bad}
%      ,{"Luke Skywalker", 24, male, good}].
```

Great but what about a more complex relationship?

```prolog
assert(tag("John Smith", "Matrix")).
assert(tag("John Smith", "Glasses")).
assert(tag("John Smith", "Machine")).
assert(tag("Bill Kill", "Katana")).
assert(tag("Bill Kill", "Five Fingers Death Punch")).
assert(tag("Luke Skywalker", "Jedi")).
assert(tag("Luke Skywalker", "Light Saber")).
```

Based on the previous call, we can list user names and their tags, but
it will give you lot of repetition.

```prolog
findall({Name, Tag}, (user(Name, _,_), tag(Name, Tag)), Xs).
% Xs = [{"John Smith", "Matrix"}, {"John Smith", "Glasses"}
%      ,{"John Smith", "Machine"}, {"Bill Kill", "Katana"}
%      ,{"Bill Kill", "Five Fingers Death Punch"}
%      ,{"Luke Skywalker", "Jedi"}, {"Luke Skywalker", "Light Saber"}].
```

What we want is a join.

```prolog
assert(
  user_tags(Name, Tags) :-
    findall(Tag, (user(Name, _,_), tag(Name, Tag)), Tags)
).
user_tags("Jedi", "Light Saber").

% or with aggregate_all
assert(
  user_tags(Name, Tags) :-
    aggregate_all(set(Tag)
                 ,(user(Name,_,_), tag(Name, Tag))
                 ,Tags)
).

% tags per users
assert(
  users_tags(Result) :-
    findall( {Name, Tags}
           , aggregate( set(Tag)
                      , (user(Name,_,_), tag(Name, Tag))
                      , Tags)
           , Result)
).

users_tags(Xs).
%  Xs = [ {"Bill Kill", ["Five Fingers Death Punch", "Katana"]}
%       , {"John Smith", ["Glasses", "Machine", "Matrix"]}
%       , {"Luke Skywalker", ["Jedi", "Light Saber"]}
%       ].
```

To remove one or more entry, `retract/1`[^prolog-retract/1] can be
used.

```prolog
retract(user("Bill Kill", X, _).
% X = 42
```

To purge all data, `abolish/1`[^prolog-abolish/2] can be used.

```prolog
abolish(user, 3).
```

These previous action on the knowledge base are not safe but
transaction[^prolog-transaction] can be used instead.


[^prolog-wikipedia]: [https://en.wikipedia.org/wiki/Prolog](https://en.wikipedia.org/wiki/Prolog)
[^prolog-assert/1]: [https://www.swi-prolog.org/pldoc/doc_for?object=assert/1](https://www.swi-prolog.org/pldoc/doc_for?object=assert/1)
[^prolog-findall/3]: [https://www.swi-prolog.org/pldoc/doc_for?object=findall/3](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3)
[^prolog-retract/1]: [https://www.swi-prolog.org/pldoc/doc_for?object=retract/1](https://www.swi-prolog.org/pldoc/doc_for?object=retract/1)
[^prolog-abolish/2]: [https://www.swi-prolog.org/pldoc/doc_for?object=abolish/2](https://www.swi-prolog.org/pldoc/doc_for?object=abolish/2)
[^prolog-transaction]: [https://www.swi-prolog.org/pldoc/man?section=transactions](https://www.swi-prolog.org/pldoc/man?section=transactions)

### From Prolog to Datalog

> Datalog is a declarative logic programming language. While it is
> syntactically a subset of Prolog, Datalog generally uses a bottom-up
> rather than top-down evaluation model. This difference yields
> significantly different behavior and properties from Prolog. It is
> often used as a query language for deductive databases. Datalog has
> been applied to problems in data integration, networking, program
> analysis, and more.[^datalog-wikipedia]

Let create something similar than the example created with prolog. To
create a relation in cozoscript the `:create` function is used,
followed by the name of the relation and its spec. The fields before
the arrow are the primary key, in this case, `name`.

```cozoscript
:create user {
  name: String,
  =>
  age: Int,
  password: String
}
```

`character` relation.

```cozoscript
:create character {
  name: String,
  =>
  sex: String,
  alignment: String
}
```

`tag` relation.

```cozoscript
:create tag {
  name: String,
  user: String
}
```

Same can be execute once using multi-transaction syntax.

```cozoscript
{
  :create user {
    name: String,
    =>
    age: Int,
    password: String
  }
}

{
  :create character {
    name: String,
    =>
    sex: String,
    alignment: String
  }
}

{
  :create tag {
    name: String,
    tag: String
  }
}

```

These relations can be listing using `::relations` system operator.

```cozoscript
::relations
```

| `name`    | `arity` | `access_levpel` | `n_keys` | `n_non_keys` | `n_put_triggers` | `n_rm_triggers` | `n_replace_triggers` | `description` |
| :-        | - | -     | - | - | - | - | - | - |
| character | 3 | normal | 1 | 2 | 0 | 0 | 0 |
| tag       | 2 | normal | 2 | 0 | 0 | 0 | 0 |
| user      | 3 | normal | 1 | 2 | 0 | 0 | 0 |

Columns can be listing using `::columns` system operator.

```cozoscript
::columns user
```

| `column` | `is_key` | `index` | `type` | `has_default` |
| :-       | -        | -       | -      | -             |
| name     | true     | 0       | String | false         |
| age      | false    | 1       | Int    | false         |
| password | false    | 2       | String | false         |

Data injection.

```cozoscript
{
  ?[name, age, password] <- [
    ['John Smith', 42, 'StrongPassword'],
    ['Bill Kill', 57, 'Beatrix'],
    ['Luke Skywalker', 24, 'IHateBrenda']
  ]
  :put user { name => age, password }
}

{
  ?[name, sex, alignment] <- [
    ['John Smith', 'male', 'bad'],
    ['Bill Kill', 'male', 'bad'],
    ['Luke Skywalker', 'male', 'good']
  ]
  :put character { name => sex, alignment }
}

{
  ?[name, tag] <- [
    ['John Smith', 'Matrix'],
    ['John Smith', 'Glasses'],
    ['John Smith', 'Machine'],
    ['Bill Kill', 'Katana'],
    ['Bill Kill', 'Five Fingers Death Punch'],
    ['Luke Skywalker', 'Jedi'],
    ['Luke Skywalker', 'Light Saber']
  ]
  :put tag { name, tag }
}
```

List all data from `user` relation

```cozoscript
?[name, age, password] := *user[name, age, password]
```

```cozoscript
::explain { ?[name] := *user[name, age, password] }
```

| `stratum` | `rule_idx` | `rule` | `atom_idx` | `op` | `ref` | `joins_on` | `filters/expr` | `out_relation` |
| - | - | - | - | ---         | --    | --   | --   | ---                           |
| 0 | 0 | ? | 1 | load_stored | :user | null | []   | `["name", "age", "password"]` |
| 0 | 0 | ? | 0 | out         | null  | null | null | `["name"]` |

```cozoscript
?[name, sex, alignment] := *character[name, sex, alignment]
```

| name           | sex  | alignment |
| :-             | -    | -         |
| Bill Kill      | male | bad       |
| John Smith     | male | bad       |
| Luke Skywalker | make | good      |

```cozoscript
?[name, tag] := *tag[name, tag]
```

| name | tag |
| -    | -   |
|      |     |

```cozoscript
?[name, tag, age] := *tag[name, tag],
                     *user[name, age, password],
                     name == 'Bill Kill'
```

| name | tag |
| -    | -   |
| Bill Kill | Five Fingers Death Punch | 57 |
| Bill Kill | Katana                   | 57 |

```cozoscript
::explain { ?[name, tag, age] := *tag[name, tag],
                                 *user[name, age, password],
                                 name == 'Bill Kill'
}
```

[^datalog-wikipedia]: [https://en.wikipedia.org/wiki/Datalog](https://en.wikipedia.org/wiki/Datalog)

## A Quick Overview of CozoDB

 - [ ] using CozoDB REPL
 - [ ] describe and use Cozoscript syntax

> A FOSS embeddable, transactional, relational-graph-vector database,
> across platforms and languages, with time travelling capability,
> perfect as the long-term memory for LLMs and
> AI.[^cozo-official-website]

[^cozo-official-website]: [https://www.cozodb.org/](https://www.cozodb.org/)

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

`atom_ok()` function creates an atom `ok`.

```c
ERL_NIF_TERM atom_ok(ErlNifEnv *env) {
  const char* atom = "ok";
  return enif_make_atom(env, atom);
}
```

`atom_error()` function creates an atom `error`.

```c
ERL_NIF_TERM atom_error(ErlNifEnv *env) {
  const char* atom = "error";
  return enif_make_atom(env, atom);
}
```

### `open_db` function

The first interface implemented was created for
`cozo_open_db()`[^libcozo-opendb] and called `open_db`.

```c
static ERL_NIF_TERM open_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

The first part of the code extract the length of each strings from
Erlang.

```c
  unsigned int engine_length;
  if (!enif_get_string_length(env, argv[0], &engine_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  unsigned int path_length;
  if (!enif_get_string_length(env, argv[1], &path_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }
  
  unsigned int options_length;
  if (!(enif_get_string_length(env, argv[2], &options_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

Extract the engine string, only "mem", "sqlite" and "rocksdb" are
supported.

```c
  // extract the engine string
  char *engine = enif_alloc(engine_length);
  if (!(enif_get_string(env, argv[0], engine, engine_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

Extract the path string, a valid and existing path from the
filesystem.

```c
  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

Extract the engine options, a valid JSON object.

```c
  char *options = enif_alloc(options_length);
  if (!(enif_get_string(env, argv[2], options, options_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

Create the database and return its idea in case of success, else
returns an error with the reason "open_error".

```c
  int db_id;
  if (!cozo_open_db(engine, path, options, &db_id)) {
    return enif_make_tuple2(env, atom_ok(env), enif_make_int(env, db_id));
  }
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "open_error"));  
}
```

[^libcozo-opendb]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L23](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L23)

### `close_db` function

`close_db()` function is an interface to
`cozo_close_db()`[^libcozo-close].

```c
static ERL_NIF_TERM close_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

```c
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
```

```c
  bool close_result = cozo_close_db(db_id);
  if (close_result) {
    return atom_ok(env);
  }
```

```c
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "close_error"));
}
```

[^libcozo-close]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L37](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L37)

### `run_query` function

`run_query()` function is an interface to
`cozo_run_query()`[^libcozo-runquery].

```c
static ERL_NIF_TERM run_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

```c
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
```

```c
  unsigned int script_raw_length;
  if (!enif_get_string_length(env, argv[1], &script_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }
```

```c
  unsigned int params_raw_length;
  if (!enif_get_string_length(env, argv[2], &params_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }
```

```c
  int immutable;
  if (!enif_get_int(env, argv[3], &immutable)) {
    return enif_make_badarg(env);
  }
```

```c
  char *script_raw = enif_alloc(script_raw_length);
  if (!(enif_get_string(env, argv[1], script_raw, script_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

```c
  char *params_raw = enif_alloc(params_raw_length);
  if (!(enif_get_string(env, argv[2], params_raw, params_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

```c
  char *cozo_result = cozo_run_query(db_id, script_raw, params_raw,
                                     immutable ? true : false);
```

```c
  ERL_NIF_TERM result_string = enif_make_string(env, cozo_result, ERL_NIF_UTF8);
```

```c
  cozo_free_str(cozo_result);
```

```c
  return enif_make_tuple2(env, atom_ok(env), result_string);
}
```

[^libcozo-runquery]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L47](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L47)

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
Match Specifications[^erlang-match-specification] in Erlang. Something
similar can be found in clojure datahike[^datahike] database.

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
[^datahike]: [https://github.com/replikativ/datahike](https://github.com/replikativ/datahike)
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

## ANNEXE A - Manual Building

```sh
git clone https://github.com/cozodb/cozo.git
cd cozo
git checkout v0.7.2
cargo build -p cozo --release --verbose
```

```
# release build:
file target/release/cozo-bin
file target/release/libcozo_c.so
file target/release/libcozo_embedded.so

# debug build:
file target/debug/libcozo_c.so
file target/debug/libcozo_embedded.so
```

```sh
cargo build -p cozo-bin --release
./target/debug/cozo-bin repl
```

## ANNEXE B - Building on OpenBSD

At this time, cozodb does not work on OpenBSD because of missing
support on `nanorand` crate.

```sh
git clone https://github.com/cozodb/cozo.git
cd cozo
git checkout v0.7.2
cargo build -p cozo --release
```

Unfortunately, it fails with this error:

```
error[E0425]: cannot find function `backup_entropy` in this scope
  --> /home/user/.cargo/registry/src/index.crates.io-6f17d22bba15001f/nanorand-0.7.0/
       src/entropy.rs:51:2
   |
51 |     backup_entropy(out);
   |     ^^^^^^^^^^^^^^ not found in this scope

For more information about this error, try `rustc --explain E0425`.
error: could not compile `nanorand` (lib) due to previous error
warning: build failed, waiting for other jobs to finish...
```

A PR[^nanorand-rs-pr-40] has been merged in December 2022, fixing this
issue for the next version of
`nanorand`[^nanorand-rs-commit-1438]. This crate can be built locally
without issue after this patch.

```sh
cargo add --git https://github.com/Absolucy/nanorand-rs  \
  --package cozo --rev 1438c12483b58245a86c87df38e71ca1e023dedc nanorand
```

[^nanorand-rs-pr-40]: [https://github.com/Absolucy/nanorand-rs/pull/40](https://github.com/Absolucy/nanorand-rs/pull/40)
[^nanorand-rs-commit-1438]: [https://github.com/Absolucy/nanorand-rs/commit/1438c12483b58245a86c87df38e71ca1e023dedc](https://github.com/Absolucy/nanorand-rs/commit/1438c12483b58245a86c87df38e71ca1e023dedc)

## ANNEXE C - CozoDB Example with C

Here a small text created during the first tests.

```c
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include "cozo_c.h"

int main(int argc, char *argv[]) {
  // create a new id
  int32_t id;

  // create an error string
  char *t;

  // open a new db as mem one
  t = cozo_open_db("mem", "/tmp/t.db", "", &id);

  // allocate 1024 byte and set them to zero
  char *data = malloc(1024);
  bzero(data, 1024);

  // execute a query
  data = cozo_run_query(id, "?[] <- [[1, 2, 3]]", "", true);
  printf("%s\n", data);

  // backup the database
  cozo_backup(id, "/tmp/t2.db");

  // free up memory
  // in fact, we should use cozo_free_str.
  free(data);

  // close the database
  bool ret = cozo_close_db(id);
  return 0;
}
```

## ANNEXE D - Alternative Datalog Implementation

| name | language | 
| :-   | -        |
| [HarvardPLAbcDatalog](https://github.com/HarvardPL/AbcDatalog)  | Java    |
| [fogfish/datalog](https://github.com/fogfish/datalog)           | Erlang  |
| [travitch/datalog](https://github.com/travitch/datalog)         | Haskell |
| [racket/datalog](https://github.com/racket/datalog)             | Racket  |
| [souffle-lang/souffle](https://github.com/souffle-lang/souffle) | C++     |
| [ekzhang/crepe](https://github.com/ekzhang/crepe)               | Rust    |
| [c-cube/datalog](https://github.com/c-cube/datalog)             | Ocaml   |
| [google/mangle](https://github.com/google/mangle)               | Go      |
| [dave-nachman/datalog](https://github.com/dave-nachman/datalog) | Python  |
| [catwell/datalog.lua](https://github.com/catwell/datalog.lua)   | Lua     |
| [tonsky/datascript](https://github.com/tonsky/datascript)       | Clojure |
| [EvgSkv/logica](https://github.com/EvgSkv/logica)               | Python  |
| [juji-io/datalevin](https://github.com/juji-io/datalevin)       | Clojure |
| [rust-lang/datafrog](https://github.com/rust-lang/datafrog)     | Rust    |
| [replikativ/datahike](https://github.com/replikativ/datahike)   | Clojure |


 - https://amnesia.sourceforge.net/user_manual/manual.html
 - https://www.erlang.org/doc/man/qlc.html#
