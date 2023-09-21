---
status: draft
date: 2023-09-13
title: Integrating CozoDB in Erlang Ecosystem
subtitle: Mixing NIF Interfaces, CozoDB, Datalog and Erlang Together
author: Mathieu Kerjouan
keywords: [cozo,cozodb,erlang,otp,datalog]
license: CC BY-NC-ND
abstract: |

  Erlang/OTP has been created with an incredible toolbox, including
  different application to store data. `ETS`, an in-memory Erlang term
  storage used as cache; `DETS`, an long term on disk storage facility
  based on ETS and finally, `Mnesia`, a database using `ETS` and
  `DETS` to create fully distributed *DBMS*.

  All these applications are not coming from obscure external projects
  but are *de facto* delivered by default with each Erlang/OTP
  releases. Thus, Erlang community is also providing some great
  alternatives, in particular with the famous *Riak* or with the
  integration of *RocksDB* -- an high performance database created by
  *Facebook*.

  Those solutions are unfortunatelly nothing in regard of SQL. This
  standard is probably the most used and advanced languages when
  dealing with relation between data but this is not the only
  one. Indeed, SQL was greatly inspired by *Prolog* and *Datalog*, two
  languages made in the 70's, created and designed to easily creates
  data and build relation between them.

  In this paper, an integration of CozoDB *v0.7.2* -- a database using
  a Datalog dialect -- is presented using its C interface called
  `libcozo_c`.

---

## Introduction

> Prolog is a logic programming language associated with artificial
> intelligence and computational linguistics. Prolog has its roots in
> first-order logic, a formal logic, and unlike many other programming
> languages, Prolog is intended primarily as a declarative programming
> language: the program logic is expressed in terms of relations,
> represented as facts and rules. A computation is initiated by
> running a query over these relations.[^prolog-wikipedia]

> Datalog is a declarative logic programming language. While it is
> syntactically a subset of Prolog, Datalog generally uses a bottom-up
> rather than top-down evaluation model. This difference yields
> significantly different behavior and properties from Prolog. It is
> often used as a query language for deductive databases. Datalog has
> been applied to problems in data integration, networking, program
> analysis, and more.[^datalog-wikipedia]

*Datalog* is greatly inspired from *Prolog*, and because many
*Datalog* implementation are not free or open-source, this part of the
article will try to explain the concept behind Datalog using
*Prolog*. When programming with Logic Programming languages like *Prolog*,
a *knowledge base* is accessible during the execution of the
program. You can see it as a database containing terms for the
moment. If you can add terms, you can also design them. In the
following examples, [SWI
Prolog](https://www.swi-prolog.org/)[^swi-prolog] REPL will be used by
invocaking the command `swipl`.

```sh
swipl
```

Say someone wants to store users into the *knowledge based*. An user
is defined by its name, its age and its password. An entry is called a
fact, and we can add new one using
[`assert/1`](https://www.swi-prolog.org/pldoc/doc_for?object=assert/1)[^prolog-assert/1]
followed by the definition of a new *fact*. Let adds three new user,
*John Smith*, *Bill Kill* and *Luke Skywalker*.

```prolog
assert(user("John Smith", 42, "StrongPassword")).
assert(user("Bill Kill", 57, "Beatrix")).
assert(user("Luke Skywalker", 24, "IHateBranda")).
```

Directly from the *REPL*, one can easily extract the password by
creating a query using `user/3` predicate composed by the name of the
user as a static value, an empty variable and a named variable called
`Password`. This last will return the content of the password field
from the user.

```prolog
user("John Smith", _, Password).
% Password = "Strong Password"
```

More than one *facts* have been added into the knowledge base, and using
[`findall/3`](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3)[^prolog-findall/3]
predicate, we can easily extract all the user name from it.

```prolog
findall(Name, user(Name, _, _), Xs).
% Xs = ["John Smith", "Bill Kill", "Luke Skywalker"].
```

A *guard* can also be used to used to filter result and extract the
wanted data.

```prolog
findall(Name, (user(Name, Age, _), Age>40), Xs).
% Xs = ["John Smith", "Bill Kill"].
```

It's also possible to create more complex data-structures by composing
data together from facts return by
[`findall/3`](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3).


```prolog
findall({Name, Password}, (user(Name, Age, Password)), Xs).
% Xs = [{"John Smith", "Strong Password"}
%     ,{"Bill Kill", "Beatrix"}
%     ,{"Luke Skywalker", "IHateBranda"}].
```

Extract the whole database or all predicates from the database is also
an easy task.

```prolog
findall( user(Name, Age, Password)
       , (user(Name, Age, Password))
       , Xs).
% Xs = [user("John Smith", 42, "Strong Password")
%      ,user("Bill Kill", 57, "Beatrix")
%      ,user("Luke Skywalker", 24, "IHateBranda")].
```

A predicate can be extended, but it will probably have many side
effects on the whole interfaces used by developers. In fact, it is
also possible to extend a predicate by creating a new one containing
an explicit reference to the old one.

```prolog
assert(character("John Smith", male, bad)).
assert(character("Bill Kill", male, bad)).
assert(character("Luke Skywalker", male, good)).
```

We can easily merge both table together using
[`findall/3`](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3)[^prolog-findall/3]. Indeed,
this is equivalent to *join* in *SQL*, elements from user and
character predicates are now combined on the same data-structure,
using name as primary key.

```prolog
findall( {Name, Age, Sex, Type}
       , ( user(Name, Age, _Password)
         , character(Name, Sex, Type))
       , Xs).
% Xs = [{"John Smith", 42, male, bad}
%      ,{"Bill Kill", 57, male, bad}
%      ,{"Luke Skywalker", 24, male, good}].
```

More complex relationship can also be created. A `tag` predicate can
be created where one user can have many tags.

```prolog
assert(tag("John Smith", "Matrix")).
assert(tag("John Smith", "Glasses")).
assert(tag("John Smith", "Machine")).
assert(tag("Bill Kill", "Katana")).
assert(tag("Bill Kill", "Five Fingers Death Punch")).
assert(tag("Luke Skywalker", "Jedi")).
assert(tag("Luke Skywalker", "Light Saber")).
```

Based on the previous call, we can list user name and their tags, but
it will give you lot of repetition. One user should have zero to many
tags, the best data-structure to answer this issue is a list.

```prolog
findall( {Name, Tag}
       , (user(Name, _,_)
         ,tag(Name, Tag))
       , Xs).
% Xs = [{"John Smith", "Matrix"}, {"John Smith", "Glasses"}
%      ,{"John Smith", "Machine"}, {"Bill Kill", "Katana"}
%      ,{"Bill Kill", "Five Fingers Death Punch"}
%      ,{"Luke Skywalker", "Jedi"}, {"Luke Skywalker", "Light Saber"}].
```

What we want is a join.

```prolog
assert(
  user_tags(Name, Tags) :-
    findall( Tag
           , ( user(Name, _,_)
             , tag(Name, Tag))
           , Tags)
).
% user_tags("Jedi", "Light Saber").
```

```prolog
assert(
  user_tags(Name, Tags) :-
    aggregate_all( set(Tag)
                 , ( user(Name,_,_)
                   , tag(Name, Tag))
                 , Tags)
).
```

[`aggregate/3`](https://www.swi-prolog.org/pldoc/doc_for?object=aggregate/3)[^prolog-aggregate/3]
predicate can be coupled with
[`findall/3`](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3)[^prolog-findall/3].

```prolog
% tags per users
assert(
  users_tags(Result) :-
    findall( {Name, Tags}
           , aggregate( set(Tag)
                      , ( user(Name,_,_)
                        , tag(Name, Tag))
                      , Tags)
           , Result)
).

users_tags(Xs).
%  Xs = [ {"Bill Kill", ["Five Fingers Death Punch", "Katana"]}
%       , {"John Smith", ["Glasses", "Machine", "Matrix"]}
%       , {"Luke Skywalker", ["Jedi", "Light Saber"]}
%       ].
```

To remove one or more entry,
[`retract/1`](https://www.swi-prolog.org/pldoc/doc_for?object=retract/1)[^prolog-retract/1]
can be used.

```prolog
retract(user("Bill Kill", X, _).
% X = 42
```

To purge all data,
[`abolish/1`](https://www.swi-prolog.org/pldoc/doc_for?object=abolish/2)[^prolog-abolish/2]
can be used.

```prolog
abolish(user, 3).
```

These previous action on the knowledge base are not safe but
transaction[^prolog-transaction] can be used instead.

[^swi-prolog]: [https://www.swi-prolog.org/](https://www.swi-prolog.org/)
[^prolog-wikipedia]: [https://en.wikipedia.org/wiki/Prolog](https://en.wikipedia.org/wiki/Prolog)
[^prolog-assert/1]: [https://www.swi-prolog.org/pldoc/doc_for?object=assert/1](https://www.swi-prolog.org/pldoc/doc_for?object=assert/1)
[^prolog-findall/3]: [https://www.swi-prolog.org/pldoc/doc_for?object=findall/3](https://www.swi-prolog.org/pldoc/doc_for?object=findall/3)
[^prolog-retract/1]: [https://www.swi-prolog.org/pldoc/doc_for?object=retract/1](https://www.swi-prolog.org/pldoc/doc_for?object=retract/1)
[^prolog-abolish/2]: [https://www.swi-prolog.org/pldoc/doc_for?object=abolish/2](https://www.swi-prolog.org/pldoc/doc_for?object=abolish/2)
[^prolog-transaction]: [https://www.swi-prolog.org/pldoc/man?section=transactions](https://www.swi-prolog.org/pldoc/man?section=transactions)
[^datalog-wikipedia]: [https://en.wikipedia.org/wiki/Datalog](https://en.wikipedia.org/wiki/Datalog)
[^prolog-aggregate/3]: [https://www.swi-prolog.org/pldoc/doc_for?object=aggregate/3](https://www.swi-prolog.org/pldoc/doc_for?object=aggregate/3)

## A Quick Overview of CozoDB

> A FOSS embeddable, transactional, relational-graph-vector database,
> across platforms and languages, with time travelling capability,
> perfect as the long-term memory for LLMs and
> AI.[^cozo-official-website]

CozoDB is a modern *Datalog* system supporting *in-memory*, *sqlite*
and *rocksdb* back-end storage. Written in Rust, it offers also a
command-line interface called `cozo-bin`, a C library called
`cozo-lib-c` or `libcozo_c`, a Java library called `cozo-lib-java`, a
NodeJS library called `cozo-lib-nodejs`, and two others for python
called `cozo-lib-python` and for swift called `cozo-lib-swift`. CozoDB
can also be compiled as web assembly library with the project
`cozo-lib-wasm`.

CozoDB offers a scripting language called *Cozoscript* to interact
with the database. The syntax is inspired from *Prolog* and *Datalog*,
but with many differences. The scripts in this paragraph can easily be
executed on any modern web browsers in the [*CozoDB
Playground*](https://www.cozodb.org/wasm-demo/)[^cozodb-playground].

Let create something similar than the example created with prolog
using cozodb. *Relation* is the name used to define a *table* in
cozodb, and can be created using `:create` followed by two arguments,
the first one is the name of the relation and then the specification
of each columns. Relations `user` and `character` are defined in code
below.

```cozoscript
:create user {
  name: String,
  =>
  age: Int,
  password: String
}
```

```cozoscript
:create character {
  name: String,
  =>
  sex: String,
  alignment: String
}
```

The arrow `=>` separates the primary key from standard one. All
columns are typed and can be set as `Null`, `Bool`, `Number` (`Int` or
`Float`), `String`, `Bytes`, `Uuid`, `List`, `Vector`, `Json` or
`Validity`. Without an arrow `=>` all the keys are consired primary,
like in the `tag` relation definition.

```cozoscript
:create tag {
  name: String,
  user: String
}
```

All the previous code presented must be executed one at a time. If
someone want to execute in a specific order and in the same session,
each queries must be isolated in command blocks, enclosed in curly
brackets (`{` and `}`). A multiline query must start with a space.


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

CozoDB offers also few commands to manage the database. To return the
list of created *relations*, `::relations` system command can be used.

```cozoscript
::relations
```

It will return a complete description of each *relations*.

| `name`        | `arity`        | `access_ levpel` | `n_ keys` | `n_ non_ keys` | `n_ put_ triggers` | `n_ rm_ triggers` | `n_ replace_ triggers` | `description` |
| :-            | - | -          | - | - | - | - | - | - |
| `"character"` | `3` | `"normal"` | `1` | `2` | `0` | `0` | `0` | `""`
| `"tag"`       | `2` | `"normal"` | `2` | `0` | `0` | `0` | `0` | `""`
| `"user"`      | `3` | `"normal"` | `1` | `2` | `0` | `0` | `0` | `""`

Same can be done for all *columns* defined in a *relation* with the
help of `::colums` system command.

```cozoscript
::columns user
```

Another table is returned, but this time with the description of each
*columns*.

| `column`     | `is_key`   | `index`   | `type`   | `has_default` |
| :-           | -          | -         | -        | -             |
| `"name"`     | `true`     | `0`       | `String` | `false`       |
| `"age"`      | `false`    | `1`       | `Int`    | `false`       |
| `"password"` | `false`    | `2`       | `String` | `false`       |


Data can be injected in *relations* with `:put` command. A list of raw
data is serialized as list and then matched on the left side of the
query. Elements are then pushed into the wanted *relation* using
`:put`.

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

All data stored in a relation can be listed using a *query* composed
in two parts. An inline rule can be used here, the part on the left
will extract the data from the right part following the `:=` operator.

```cozoscript
?[name, age, password] := *user[name, age, password]
```

| name               | age | password           |
|                 -: |  -: |                 -: |
| `"Bill Kill"`      |  57 |        `"Beatrix"` |
| `"John Smith"`     |  42 | `"StrongPassword"` |
| `"Luke Skywalker"` |  24 |    `"IHateBrenda"` |

This query can be analyzed and explained using `::explain` command. It
will return each step of executions.

```cozoscript
::explain { ?[name] := *user[name, age, password] }
```

| `stratum` | `rule_idx` | `rule` | `atom_idx` | `op` | `ref` | `joins_on` | `filters/expr` | `out_relation` |
| - | - | - | - | ---         | --    | --   | --   | ---                           |
| `0` | `0` | `?` | `1` | `load_stored` | `:user` | `null` | `[]`   | `["name", "age", "password"]` |
| `0` | `0` | `?` | `0` | `out`         | `null`  | `null` | `null` | `["name"]` |

Let prints out the content of `character` *relation*...

```cozoscript
?[name, sex, alignment] := *character[name, sex, alignment]
```

| name               | sex      | alignment |
| :-                 | -        | -         |
| `"Bill Kill"`      | `"male"` | `"bad"`   |
| `"John Smith"`     | `"male"` | `"bad"`   |
| `"Luke Skywalker"` | `"male"` | `"good"`  |

... and the content of `tag` one.

```cozoscript
?[name, tag] := *tag[name, tag]
```

| name | tag |
|   -: | -: |
|      `"Bill Kill"` | `"Five Fingers Death Punch"` |
|      `"Bill Kill"` |                   `"Katana"` |
|     `"John Smith"` |                  `"Glasses"` |
|     `"John Smith"` |                  `"Machine"` |
|     `"John Smith"` |                   `"Matrix"` |
| `"Luke Skywalker"` |                     `"Jedi"` |
| `"Luke Skywalker"` |              `"Light Saber"` |

The most interesting part now is to join them together using a guard
at the end of the query. The following code will join `tag` and `user`
relation together by unifying `name` present in both *relations*. A
filter is also added to only return the user called `Bill Kill`.

```cozoscript
?[name, tag, age] := *tag[name, tag],
                     *user[name, age, password],
                     name == 'Bill Kill'
```

The result shows how boths relations have been merged.

| name | tag | age |
|   -: | -: |  -: |
| `"Bill Kill"` | `"Five Fingers Death Punch"` | `"57"` |
| `"Bill Kill"` |                   `"Katana"` | `"57"` |

This query can be explain by using, again, `::explain` command.

```cozoscript
::explain { ?[name, tag, age] := *tag[name, tag],
                                 *user[name, age, password],
                                 name == 'Bill Kill'
}
```

Table returned is more complex than the previous ones. On the third
line, a relation is made using `name` field.

| stratum | `rule_idx` | `rule` | `atom_idx` | `op`                   | `ref`     | `joins_on`       | `filters/expr`                | `out_relation` |
|      -: |         -: |     -: |         -: |                  ----: |       --: |             ---: |                         ----: | ----: |
| 0       | 0          | `"?"`  | `3`        | `"load_stored"`        | `":tag"`  | `null`           | `["eq(name, \"Bill Kill\") "]` | `["name", "tag"]` |
| 0       | 0          | `"?"`  | `2`        | `"load_stored"`        | `":user"` | `null`           | `[]`                          | `["**0", "age", "password"]` |
| 0       | 0          | `"?"`  | `1`        | `"stored_prefix_join"` | `null`    | `{ "name": "**0" }` | `null`                        | `["name", "tag", "age"]` |
| 0       | 0          | `"?"`  | `0`        | `"out"`                | `null`    | `null`           | `null`                        | `["name", "tag", "age"]` |

`<-` is in fact a syntactic sugar made to simplify the use of the
curly tail `<~` denoting a fixed rule but that's another level of
complexity someone interested can check directly on the [official
tutorial](https://docs.cozodb.org/en/latest/tutorial.html)[^cozodb-tutorial].

[^cozo-official-website]: [https://www.cozodb.org/](https://www.cozodb.org/)
[^cozodb-playground]: [https://www.cozodb.org/wasm-demo/](https://www.cozodb.org/wasm-demo/)
[^cozodb-tutorial]: [https://docs.cozodb.org/en/latest/tutorial.html](https://docs.cozodb.org/en/latest/tutorial.html)

## CozoDB Interface with `libcozo_c`

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

`ei.h`[^erlang-ei] and `erl_nif.h`[^erlang-erl_nif] C headers are both
required.

```c
#include <ei.h>
#include <erl_nif.h>
```

`cozo_c.h`[^erlang-cozo_c.h] is present in `c_src` directory but can be
automatically fetched and upgraded using `make deps` if this file is
not present.

```c
#include "cozo_c.h"
```

Because this implementation is using an external library, the
definition of each exported functions from this one needs to be
created prefixed with the keyword `extern` meaning "external
references" outside of the scope of this program. It's basically a
copy/paste from `cozo_c.h` prototypes.

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

`ERL_NIF_INIT`[^erlang-ERL_NIF_INIT] macro is needed to generate other
internal functions and configure the NIF correctly. The first argument
is the name of the module called `cozo_nif`, following the name of the
C function `nif_funcs` returning references to the interface to
CozoDB. All other arguments are disabled, but represent the function
used when loading the library, next argument is ignored, the fifth
argument is a function called when an upgrade is made and the last one
when this module is unloaded.

```c
ERL_NIF_INIT(cozo_nif,nif_funcs,NULL,NULL,NULL,NULL)
```

The list of functions and their mapping on the Erlang side is defined
in `ErlNifFunc`. 8 functions are exported.

```c
static ErlNifFunc nif_funcs[] = {
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

`atom_ok()` and `atom_error()` functions have been created to help
generating atoms using `enif_make_atom()`[^erlang-enif_make_atom],
returning respectively the atom `ok` for the first one and the atom
`error` for the last one.

```c
ERL_NIF_TERM atom_ok(ErlNifEnv *env) {
  const char* atom = "ok";
  return enif_make_atom(env, atom);
}

ERL_NIF_TERM atom_error(ErlNifEnv *env) {
  const char* atom = "error";
  return enif_make_atom(env, atom);
}
```

[^erlang-ei]: [https://www.erlang.org/doc/man/ei.html](https://www.erlang.org/doc/man/ei.html)
[^erlang-erl_nif]: [https://www.erlang.org/doc/man/erl_nif.html](https://www.erlang.org/doc/man/erl_nif.html)
[^erlang-cozo_c.h]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h)
[^erlang-ERL_NIF_INIT]: [https://www.erlang.org/doc/man/erl_nif.html#initialization](https://www.erlang.org/doc/man/erl_nif.html#initialization)
[^erlang-enif_make_atom]: [https://www.erlang.org/doc/man/erl_nif#enif_make_atom](https://www.erlang.org/doc/man/erl_nif#enif_make_atom)

### `open_db` function

The first interface implemented was created for
`cozo_open_db()`[^libcozo-opendb] and called `open_db`.

```c
static ERL_NIF_TERM open_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

The first part of the code extract the length of each strings from
Erlang with `enif_get_string_length()`[^erlang-enif_get_string_length]
function. In case of failure, the function return a `badarg` atom with
the help of function `enif_make_badarg()`[^erlang-enif_make_badarg].

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

No error so far in the code, then the *engine* string can be extracted
with `enif_get_string()`[^erlang-enif_get_string] function. This must
be a C string and it should be terminated by `\0` but a `\n` do the
job. `mem`, `sqlite` and `rocksdb` are the only engine supported for
the moment. In case of failure a `badarg` atom is returned.

```c
  // extract the engine string
  char *engine = enif_alloc(engine_length);
  if (!(enif_get_string(env, argv[0], engine, engine_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

The *path* and *engine options* are extracted using the same
process. *path* argument must be a valid path and *engine option* must
be a string containing a valid JSON object.

```c
  char *path = enif_alloc(path_length);
  if (!(enif_get_string(env, argv[1], path, path_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *options = enif_alloc(options_length);
  if (!(enif_get_string(env, argv[2], options, options_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

An integer is allocated to store the future database id generated by
`cozo_open_db()` function. In case of success, this identifier is
returned in a classical Erlang *tuple*, where the first element is an
`ok` atom and the second one an integer created using
`enif_make_int()`[^erlang-enif_make_int], converting `db_id` in an
Erlang integer term.

```c
  int db_id;
  if (!cozo_open_db(engine, path, options, &db_id)) {
    return enif_make_tuple2(env, atom_ok(env), enif_make_int(env, db_id));
  }
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "open_error"));
}
```

[^erlang-enif_get_string_length]: [https://www.erlang.org/doc/man/erl_nif#enif_get_string_length](https://www.erlang.org/doc/man/erl_nif#enif_get_string_length)
[^erlang-enif_make_badarg]: [https://www.erlang.org/doc/man/erl_nif#enif_make_badarg](https://www.erlang.org/doc/man/erl_nif#enif_make_badarg)
[^erlang-enif_get_string]: [https://www.erlang.org/doc/man/erl_nif#enif_get_string](https://www.erlang.org/doc/man/erl_nif#enif_get_string)
[^erlang-enif_make_int]: [https://www.erlang.org/doc/man/erl_nif#enif_make_int](https://www.erlang.org/doc/man/erl_nif#enif_make_int)
[^libcozo-opendb]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L23](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L23)

### `close_db` function

An opened database must be closed if not used anymore. `close_db()`
function is an interface to `cozo_close_db()`[^libcozo-close] to deal
with this part of the library.

```c
static ERL_NIF_TERM close_db(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

Database identifier is fetched from arguments with the help of
`enif_get_int()`[^erlang-enif_get_int] function and stored in `db_id`
variable. As usual, in case of failure, `badarg` is returned.

```c
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
```

`cozo_close_db()` function is then called and in case of success, only
`ok` atom is returned.

```c
  bool close_result = cozo_close_db(db_id);
  if (close_result) {
    return atom_ok(env);
  }
```

In case of failure though, a tuple containing `error` atom and the
atom `close_error` is being returned.

```c
  return enif_make_tuple2(env, atom_error(env), enif_make_atom(env, "close_error"));
}
```

[^erlang-enif_get_int]: [https://www.erlang.org/doc/man/erl_nif#enif_get_int](https://www.erlang.org/doc/man/erl_nif#enif_get_int)
[^libcozo-close]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L37](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L37)

### `run_query` function

When a database is opened and active, queries can be executed. A query
is a string composed of characters and following cozoscript
syntax. `run_query()` function is an interface to
`cozo_run_query()`[^libcozo-runquery].

```c
static ERL_NIF_TERM run_query(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
```

An opened database is identified by an integer, this one is fetched
from the arguments passed to the function in `env` variable and then
stored in `db_id` variable.

```c
  int db_id;
  if (!enif_get_int(env, argv[0], &db_id)) {
    return enif_make_badarg(env);
  }
```

Both cozoscript and parameters are strings. Their length must be
extracted using `enif_get_string_length()` function and in case of
failure returning `badarg` atom.

```c
  unsigned int script_raw_length;
  if (!enif_get_string_length(env, argv[1], &script_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  unsigned int params_raw_length;
  if (!enif_get_string_length(env, argv[2], &params_raw_length, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }
```

The immutable flag is fetched as integer with `enif_get_int()`
function. In fact, a `bool` might have done the job. This value is
stored in `immutable` variable using `enif_get_int()` function.

```c
  int immutable;
  if (!enif_get_int(env, argv[3], &immutable)) {
    return enif_make_badarg(env);
  }
```

Cozoscript and parameters are then extracted with `enif_get_string()`
function and respectively stored in `script_raw` dans `params_raw`
variables.

```c
  char *script_raw = enif_alloc(script_raw_length);
  if (!(enif_get_string(env, argv[1], script_raw, script_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }

  char *params_raw = enif_alloc(params_raw_length);
  if (!(enif_get_string(env, argv[2], params_raw, params_raw_length, ERL_NIF_UTF8))) {
    return enif_make_badarg(env);
  }
```

The query is now ready to be executed with `cozo_run_query()`
function. The first argument is the database identifier, the second
one the script, the third one is for parameters and finally the last
one for the mutability of the database.

```c
  char *cozo_result = cozo_run_query(db_id, script_raw, params_raw,
                                     immutable ? true : false);
```

A result is returned as string and needs to be converted as an Erlang
term with `enif_make_string()` function.

```c
  ERL_NIF_TERM result_string = enif_make_string(env, cozo_result, ERL_NIF_UTF8);
```

`cozo_free_str` must be called to free up the space allocated by the
result of the function, this value is not needed anymore because it
was converted as Erlang term and previously stored in `result_string`
variable.

```c
  cozo_free_str(cozo_result);
```

Finally, the result is returned with an `ok` atom in a tuple.

```c
  return enif_make_tuple2(env, atom_ok(env), result_string);
}
```

[^libcozo-runquery]: [https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L47](https://github.com/cozodb/cozo/blob/v0.7.2/cozo-lib-c/cozo_c.h#L47)

## Low Level Interface with `cozo_nif` Module

`cozo_nif` Erlang module is a low level interface to `cozo_nif`
library. It offers two differents way to call this NIF, one is used to
help developers to debug their applications and the second one (all
function postfixed with `_nif`) is directly a raw access to the low
level functions.

```erlang
{ok, DbId} = cozo_nif:open_db("mem\n", "/tmp/test.db\n", "{}\n").
ok = cozo_nif:close(DbId).
```

Debugging can be activated on this module with
`logger:set_module_level/2` function. At this time, only debug message
containing simply the functions and their return is printed on the
screen.

```erlang
logger:set_module_level(cozo_nif, debug).
cozo_nif:open_db("mem\n", "/tmp/test.db\n", "{}\n").
```

```
=DEBUG REPORT==== 21-Sep-2023::12:57:41.229157 ===
#{args => ["mem\n","/tmp/test.db\n","{}\n"],
  caller => <0.348.0>,function => open_db,module => cozo_nif}
  {ok,2}
=DEBUG REPORT==== 21-Sep-2023::12:57:41.229911 ===
#{return => {ok,2},
  args => ["mem\n","/tmp/test.db\n","{}\n"],
  caller => <0.348.0>,function => open_db_nif,module => cozo_nif}
```

`cozo_nif:open_db/3` is a wrapper for `cozo_nif:open_db_nif/3`, the
one exported from the NIF. Nothing more to show on this module, this
is a very standard NIF implementation without any surprise and quite
boring.

## Generic Interface with `cozo` Module

`cozo` Erlang module is a friendly interface and at this time the main
one to interact correctly with CozoDB. The idea is to offer an high
level abstraction to the NIF with a big part of the features directly
integrated there. Lot of functions are exported to help developers to
deal with the database.

### Types and Record

This module (and others) are trying hard to be fully specified, types
and data-structures are defined in `include/cozo.hrl`.

```erlang
-type db_id()         :: 0 | pos_integer().
-type db_engine()     :: mem | sqlite | rocksdb.
-type db_path()       :: string().
-type db_options()    :: map().
-type db_parent()     :: pid().
-type db_query()      :: string() | binary() | [string(), ...].
```

`cozo` record includes the parameters passed to the database when
opened.

```erlang
-record(cozo, { id = undefined        :: undefined | db_id()
              , db_engine = mem       :: db_engine()
              , db_path = ""          :: db_path()
              , db_options = #{}      :: db_options()
              , db_parent = undefined :: undefined | db_parent()
              }).
-type cozo() :: #cozo{ db_parent :: undefined | db_parent() }.
```

### Generic Operations

A new active database can be opened with `cozo:open/0`, `cozo:open/1`,
`cozo:open/2` and `cozo:open/3` functions. These functions are
returning the previously defined record containing all important
information regarding this database.

```erlang
{ok, Db} = cozo:open().
```

`cozo:open/0` function opens a new in memory database with a random
path and default options. `cozo:open/1` lets developers select the
engine needed, a path will be automatically generated and default
options will be used. `cozo:open/2` lets developers chosing the engine
and the path but default options will be used. Finally, `cozo:open/3`
can configure all elements of the database.

```erlang
{ok, Db} = cozo:open(sqlite, "/tmp/my_path.db", #{}).
```

Many accessors have been created to extract data from `cozo` record,
like `cozo:get_id/1`, `cozo:get_options/1`, `cozo:get_path` or
`cozo:get_engine/1`.

```erlang
0 = cozo:get_id(Db).
DbOptions = cozo:get_options(Db).
DbPath = cozo:get_path(Db).
DbEngine = cozo:get_engine(Db).
```

Any opened database can be closed using `cozo:close/1` function.

```erlang
ok = cozo:close(Db).
```

### Running Queries

At this time, the library only offer a simple cozoscript interface to
CozoDB using mainly an Erlang string with `cozo:run/*` functions.

```erlang
cozo:run(3, "?[] <- [[1,2,3]]").
```

This function will return a tuple containing with a JSON parsed
result.

```erlang
{ok, #{
  <<"headers">> => [<<"_0">>,<<"_1">>,<<"_2">>],
  <<"next">> => null,
  <<"ok">> => true,
  <<"rows">> => [[1,2,3]],
  <<"took">> => 0.01818754
  }
}
```

The previous command is equivalent to the one below, where default
parameters are passed and mutability is set to `false`.

```erlang
cozo:run(Db, "?[] <- [[1,2,3]]", #{}, false).
```

`cozo:run/*` functions are used to send queries, and also system
commands. A new relation can be created using `:create` command.

```erlang
cozo:run(Db, ":create test { key: String, value: String }").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 0.038191377
%   }
% }
```

Rows can also be added as well with `:put` command.

```erlang
cozo:run(Db, "?[key, value] <- [['key','value']] :put test {key, value}").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 0.039636681
%   }
% }
```

Finally, stored rows on `test` *relation* can be extracted as well.

```erlang
cozo:run(Db, "?[key, value] := *test{key, value}").
% {ok,#{<<"headers">> => [<<"key">>,<<"value">>],
%       <<"next">> => null,<<"ok">> => true,
%       <<"rows">> => [[<<"key">>,<<"value">>]],
%       <<"took">> => 0.010064009}}
```

### Importing and Exporting Relations

*Relations* can be exported and imported. To export the full content
of a relation, `cozo:export_relations/2` can be used. A map containing
the name of the relation needs to be create in the format `#{
<<"relations>> => [R1, R2, RN]}` where `R1`, `R2` and `RN` are the
names of the relations. This function returns another map converted
from JSON object with the rows and the headers of the *relation*.

```erlang
Payload = #{ <<"relations">> => [<<"test">>] },
{ok, Result} = cozo:export_relations(Db, Payload).
% {ok, #{
%   <<"data">> => #{
%     <<"test">> => #{
%       <<"headers">> => [
%         <<"key">>,
%         <<"value">>
%       ],
%       <<"next">> => null,
%       <<"rows">> => [
%         [<<"key">>,<<"value">>]
%       ]
%     }
%   },
%   <<"ok">> => true
%  }
% }
```

Headers and rows can be imported using `cozo:import_relations/2`
function. Exported data can be reimported by extract the content of
the field `<<"data">>`and reinject them.

```erlang
#{ <<"data">> := Import } = Result.
{ok, _} = cozo:import_relations(Db, Import).
% {ok, #{
%   <<"ok">> => true
%   }
% }
```

This part needs to be improved, creating maps data-structure without
knowing what kind of fields are required is annoying and can lead to
errors.

### Database Backups and Restoration

An active database can have backups, and those backups can also be
restored. To create a backup, `cozo:backup/2` function is used, the
second argument must be a valid path.

```erlang
{ok, _} = cozo:backup(Db, "/tmp/data.dump").
```

The backup file is using sqlite format.

```sh
file /tmp/data.dump
# /tmp/data.dump: SQLite 3.x database,
#   last written using SQLite version 3039002

sqlite /tmp/data.dump
# sqlite> .schema
# CREATE TABLE cozo
#         (
#            k BLOB primary key,
#            v BLOB
# );
```

To restore a database using `cozo:restore/2`, a new empty database
needs to be created. When successfully restored, headers and rows are
back.

```erlang
{ok, DbRestore} = cozo:open().
{ok, _} = cozo:restore(DbRestore, "/tmp/data.dump").
cozo:run(4, "?[key, value] := *test{key, value}").
% {ok, #{
%   <<"headers">> => [<<"key">>,<<"value">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"key">>,<<"value">>]],
%   <<"took">> => 1.97193e-4
%   }
% }
```

Another way to do a restore is to use `cozo:import_backup/2`. This
time a map is passed with the path and the relations to import as
list of string.

```erlang
Payload = #{
  <<"path">> => <<"/tmp/data.dump">>,
  <<"relations">> => [<<"test">>]
}.
{ok, _} = cozo:import_backup(Db, Payload).
```

This interface needs to be improved, configuring map without
specification is hard and can be highly confusing.

### Managing Relations

CozoDB can manage relations with dedicated commands, at this time, the
integration of this feature is unstable and will change in the
releases. Use them with care. Most the of the system commands have
their own dedicated functions, that the case when one needs to deal
with *relations*.

```erlang
cozo:create_relation(Db, "managing_relations"
                       , "key => value").

cozo:create_relation(Db, "managing_relations2"
                       , "key: String => value: String").

cozo:create_relation(Db, "managing_relation3"
                       , [c1, c2, c3]).
```

Relations can also be listed with the help of `cozo:list_relations/1`
function, still returned as map structure from JSON object.

```erlang
{ok, Relations} = cozo:list_relations(Db).
% {ok, #{
%   <<"headers">> => [
%     <<"name">>,
%     <<"arity">>,
%     <<"access_level">>,
%     <<"n_keys">>,
%     <<"n_non_keys">>,
%     <<"n_put_triggers">>,
%     <<"n_rm_triggers">>,
%     <<"n_replace_triggers">>,
%     <<"description">>
%   ],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [
%     [<<"managing_relation3">>,3,<<"normal">>,3,0,0,0,0,<<>>],
%     [<<"managing_relations">>,2,<<"normal">>,1,1,0,0,0,<<>>],
%     [<<"managing_relations2">>,2,<<"normal">>,1,1,0,0,0,<<>>]
%   ],
%   <<"took">> => 6.6139e-5
%   }
% }
```

Finally, relations can be removed using `cozo:delete_relation/2` or
`cozo:delete_relations/2` function.

```erlang
{ok, _} = cozo:delete_relation(Db, "managing_relation3").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 2.21824e-4
%   }
% }

{ok, _} = cozo:delete_relations(Db, ["managing_relations", "managing_relations2"]).
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 5.7769e-5
%   }
% }
```

Other functions like `cozo:replace_relation/3`, `cozo:put_row/3`,
`cozo:update_row/3`, `cozo:delete_row/3`, `cozo:ensure_row/3` and
`cozo:ensure_not_row/3` need to be deeply tested and are currently not
working correctly.

### Managing Indexes

CozoDB can created indexes, at this time, the integration of this
feature is unstable and will change in the releases. Use them with
care. A new relation called `managing_index` is created for this
example.

```erlang
{ok, _} = cozo:create_relation(Db, "managing_index", "key => value").
```

An index can be created using `cozo:create_index/3` function, where
the second argument is the name of the index, and the last one is the
specification for this index, basically, the name of the column.

```erlang
{ok, _} = cozo:create_index(Db, "managing_index:index", "key").
```

All indexes can be listed on a *relation* using `cozo:list_indices/2`.

```erlang
{ok, _} = cozo:list_indices(Db, "managing_index").
% {ok, #{
%   <<"headers">> => [
%     <<"name">>,
%     <<"type">>,
%     <<"relations">>,
%     <<"config">>
%   ],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [
%     [ <<"index">>,<<"normal">>,
%       [<<"managing_index:index">>],
%       #{<<"indices">> => [0]}
%     ]
%   ],
%   <<"took">> => 7.0669e-5
%   }
% }
```

Finally, an index can also be removed using `cozo:delete_index/2`
function.

```erlang
{ok, _} = cozo:delete_index(Db, "managing_index:index").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 8.5379e-5
%   }
% }
```

### Managing Triggers

CozoDB can manage triggers with dedicated commands, at this time, the
integration of this feature is unstable and will change in the
releases. A trigger consist of an action or a query being executed
when something happens on a relation.

```erlang
{ok, Db} = cozo:open().
```

Two relations are created, `store` will received new data and
`replica` will replicated the data from `store` using triggers.

```erlang
{ok, _} = cozo:create_relation(Db, "store", "key => value").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 1.12455e-4
%   }
% }

{ok, _} = cozo:create_relation(Db, "replica", "key => value").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 1.16418e-4
%   }
% }
```

Trigger's name must reflect the name of the `relation`. In this case,
the trigger will be called `store`. The query used will fetch all new
data and put them in `replica`.

```erlang
Trigger = "on put { "
  "?[key,value] := _new[key,value]"
  ":put replica{k,v} }".
{ok, _} = cozo:set_triggers(Db, "store", Trigger).
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 1.5311e-4
%   }
% }
```

To be sure each relations are empty, a query can be executed with
`cozo:run/2`.

```erlang
{ok, _} = cozo:run(Db, "?[key, value] := *store[key, value]").
% {ok, #{
%   <<"headers">> => [<<"key">>,<<"value">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [],
%   <<"took">> => 1.81164e-4
%   }
% }

{ok, _} = cozo:run(Db, "?[key, value] := *replace[key, value]").
% {ok, #{
%   <<"headers">> => [<<"key">>,<<"value">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [],
%   <<"took">> => 1.69025e-4
%   }
% }
```

When inserting new data into `store` relation, they are replicated
into `replica` *relation* as well.

```erlang
cozo:run(Db, "?[key, value] <- [[1,2],[2,3]] :put store{key, value}").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 6.91881e-4
%   }
% }

cozo:run(Db, "?[key, value] := *store[key, value]").
% {ok, #{
%   <<"headers">> => [<<"key">>,<<"value">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[1,2],[2,3]],
%   <<"took">> => 6.1221e-4
%   }
% }

cozo:run(Db, "?[key, value] := *replica[key, value]").
% {ok, #{
%   <<"headers">> => [<<"key">>,<<"value">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[1,2],[2,3]],
%   <<"took">> => 3.38856e-4
%   }
% }
```

Triggers can be checked using `cozo:get_triggers/2`.

```erlang
{ok, _ } = cozo:get_triggers(Db, "store").
% {ok,#{
%   <<"headers">> => [<<"type">>,<<"idx">>,<<"trigger">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [
%   [<<"put">>,0,
%    <<"?[key,value] := _new[key,value]; :put replica{key,value} ">>]],
%   <<"took">> => 4.8507e-5
%   }
% }
```

Finally, triggers can be reset or deleted using
`cozo:delete_triggers/2`.

```erlang
{ok, _} = cozo:delete_triggers(Db, "store").
% {ok, #{
%   <<"headers">> => [<<"status">>],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [[<<"OK">>]],
%   <<"took">> => 1.38564e-4
%   }
% }
```

### Extra features

CozoDB can do a lot more than just dealing with data, at this time,
the integration of these features is unstable and will change in the
releases. Columns definition can be listed using
`cozo:list_columns/2`.

```erlang
{ok, _ } = cozo:list_columns(Db, "managing_index").
% {ok, #{
%   <<"headers">> => [
%     <<"column">>,
%     <<"is_key">>,
%     <<"index">>,
%     <<"type">>,
%     <<"has_default">>
%   ],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [
%     [<<"key">>,true,0,<<"Any?">>,false],
%     [<<"value">>,false,1,<<"Any?">>,false]
%   ],
%   <<"took">> => 6.2358e-5
%   }
% }
```

Queries can also be explained using `cozo:explain/2`.

```erlang
cozo:explain(Db, "?[] <- [[1,2,3]]").
% {ok, #{
%   <<"headers">> => [
%     <<"stratum">>,
%     <<"rule_idx">>,
%     <<"rule">>,
%     <<"atom_idx">>,
%     <<"op">>,
%     <<"ref">>,
%     <<"joins_on">>,
%     <<"filters/expr">>,
%     <<"out_relation">>
%   ],
%   <<"next">> => null,
%   <<"ok">> => true,
%   <<"rows">> => [
%     [0,0,<<"?">>,0,<<"algo">>,null,null,null,null]
%   ],
%   <<"took">> => 1.43279e-4
%   }
% }
```

## Isolated Interface with `cozo_db` Module

`cozo_db` was created to isolate a database behind an Erlang process
and linearize queries and answers. The idea is to offer an easy way to
synchronize and distribute queries in a cluster environment. All
interfaces are similar than the ones defined in `cozo` module.

`cozo_db:open/0`, `cozo_db:open/1`, `cozo_db:open/2` functions are
wrapper around `cozo_db:start/1` and are following the same principles
than `open` functions from `cozo` module.

```erlang
{ok, Db} = cozo_db:open().
{ok, _} = cozo_db:run(Db, "?[] <- [[1,2,3]]").
ok = cozo_db:close(Db).
```

`cozo_db` was created with `gen_statem`[^erlang-gen_statem] *behavior*
and export some `start_*` functions to start it. All these functions
are sharing the options structure defined as `proplist`.

```erlang
Opts = [
  {engine, mem},
  {db_path, "/tmp"},
  {db_filename_prefix, "cozodb_server_"},
  {path, "database_name.db"},
  {options, #{}}
].
```

`cozo_db:start/1` starts just a `gen_statem` process.

```erlang
{ok, Db} = cozo_db:start(Opts).
ok = cozo_db:stop(Db).
```

`cozo_db:start_monitor/1` starts a monitored `gen_statem` process.

```erlang
{ok, DbMonitored} = cozo_db:start_monitor(Opts).
ok = cozo_db:stop(DbMonitored).
```

`cozo_db:start_link/1` starts a linked `gen_statem` process.

```erlang
{ok, DbLinked} = cozo_db:start_link(Opts).
ok = cozo_db:stop(DbLinked).
```

CozoDB team created a pretty nice tutorial, with many use case. This
was quite useful to test the implementation and have an idea if
everything was working correctly. This tutorial used with `cozo` and
`cozo_db` modules were created using
`common_test`[^erlang-common_test].

[^erlang-common_test]: [https://www.erlang.org/doc/man/common_test.html](https://www.erlang.org/doc/man/common_test.html)
[^erlang-gen_statem]: [https://www.erlang.org/doc/man/gen_statem.html](https://www.erlang.org/doc/man/gen_statem.html)

## Future Improvements

Using *Cozoscript* to execute queries on CozoDB is enough if one wants
to create a simple user interface or a proof of concept. Users will
have their own isolated environment using something already documented
and tested.

Unfortunately, this feature is hard to integrate *correctly* with
Erlang, and by *correcty* I mean creating a request and get back a
result with Erlang terms. In fact, it is possible to create something
similar to Match Specifications[^erlang-match-specification] in
Erlang. Datahike[^datahike] project written in Clojure, for example,
did the same and the first version of Mnesia (called Amnesia) was
designed with this constraint in mind[^erlang-amnesia].

To accomplish something like this, a new interface using Rust[^rust]
and Rustler[^rustler] with `cozo-core`[^cozo-core] should be used
instead of `libcozo_c`. It will require an important amount of work
but should offer a better integration with Erlang, by letting this
langage generating CozoDB bytecode.

Another improvement, perhaps before doing bytecode compilation from
Erlang to CozoDB, is to create a fully compatible *Cozoscript*
implementation in pure Erlang. Indeed, the
specifications[^cozoscript-specifications] are freely available and
can be easily converted to something Erlang compatible with
`leex`[^erlang-leex] and `yecc`[^erlang-yecc].

An interface to `qlc`[^erlang-qlc] could also be really
helpful. Creating a similar interface than the one already offered by
*ETS* and *Mnesia* could be a great feature as well.

Finally, `erlang_nif.c` is not tested against memory leaks and other
kind of C related issues. It should be planned to create test suites
and analyze this part of the code with Valgrind[^valgrind]. Error
messages and guards must also be added to ensure a good programming
experience.

[^erlang-match-specification]: [https://www.erlang.org/doc/apps/erts/match_spec.html](https://www.erlang.org/doc/apps/erts/match_spec.html)
[^datahike]: [https://github.com/replikativ/datahike](https://github.com/replikativ/datahike)
[^erlang-amnesia]: [https://amnesia.sourceforge.net/user_manual/manual.html](https://amnesia.sourceforge.net/user_manual/manual.html)
[^rust]: [https://www.rust-lang.org/](https://www.rust-lang.org/)
[^rustler]: [https://docs.rs/crate/rustler](https://docs.rs/crate/rustler)
[^cozo-core]: [https://github.com/cozodb/cozo/tree/main/cozo-core](https://github.com/cozodb/cozo/tree/main/cozo-core)
[^cozoscript-specifications]: [https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest](https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest)
[^erlang-leex]: [https://www.erlang.org/doc/man/leex](https://www.erlang.org/doc/man/leex)
[^erlang-yecc]: [https://www.erlang.org/doc/man/yecc](https://www.erlang.org/doc/man/yecc)
[^valgrind]: [https://valgrind.org/](https://valgrind.org/)
[erlang-qlc]: [https://www.erlang.org/doc/man/qlc.html](https://www.erlang.org/doc/man/qlc.html)

## Conclusion

Integrating `libcozo_c` using Erlang NIF was easier than
anticipated. In fact, CozoDB project is well documented. The library
was nicely designed, and the interfaces were quickly created without
big trouble. Thus, one the most annoying problem was related to local
path for C libraries and headers, but this is related to LLVM or GCC
and the configuration used during this implementation. In less than a
week, this application was usable, documented, tested and
specified. Many more modifications need to be done to make this module
ready for production, but the current implementation can be used for
testing and perhaps proof of concepts. If interested to help or want
to be involved in this project, don't hesitate to contact me.

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

\newpage
## ANNEXE A - Manual Building

This procedure has been extracted from github workflow.

```sh
git clone https://github.com/cozodb/cozo.git
cd cozo
git checkout v0.7.2
cargo build -p cozo --release --verbose
```

Built release can be found in `target/release`:

 - `target/release/cozo-bin`
 - `target/release/libcozo_c.so`
 - `target/release/libcozo_embedded.so`

Built debug can be found in `target/debug`:

 - `target/debug/libcozo_c.so`
 - `target/debug/libcozo_embedded.so`

To start a new REPL can be executed with `cozo-bin`.

```sh
cargo build -p cozo-bin --release
./target/debug/cozo-bin repl
```

\newpage
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

\newpage
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

\newpage
## ANNEXE D - SQL Equivalence

Tested with SQLite.

```sql
CREATE TABLE user (
  name VARCHAR,
  age INTEGER,
  password VARCHAR
);
CREATE TABLE character (
  name VARCHAR,
  sex VARCHAR,
  alignment VARCHAR
);
CREATE TABLE tag (
  name VARCHAR,
  tag VARCHAR,
  comment VARCHAR
);

INSERT INTO user (name, age, password)
     VALUES ('John Smith', 42, 'StrongPassword');
INSERT INTO user (name, age, password)
     VALUES ('Bill Kill', 57, 'Beatrix');
INSERT INTO user (name, age, password)
     VALUES ('Luke Skywalker', 42, 'IHateBrenda');

INSERT INTO character (name, sex, alignment)
     VALUES ('John Smith', 'male', 'bad');
INSERT INTO character (name, sex, alignment)
     VALUES ('Bill Kill', 'male', 'bad');
INSERT INTO character (name, sex, alignment)
     VALUES ('Luke Skywalker', 'male', 'good');

INSERT INTO tag ( name, tag )
     VALUES ('John Smith', 'Matrix');
INSERT INTO tag ( name, tag )
     VALUES ('John Smith', 'Glasses');
INSERT INTO tag ( name, tag )
     VALUES ('Bill Kill', 'Katana');
INSERT INTO tag ( name, tag )
     VALUES ('Bill Kill', 'Five Fingers Death Punch');
INSERT INTO tag ( name, tag )
     VALUES ('Luke Skywalker', 'Jedi');
INSERT INTO tag ( name, tag )
     VALUES ('Luke Skywalker', 'Light Saber');

SELECT *
  FROM user, character
 WHERE user.name=character.name;

SELECT *
  FROM user
  JOIN character
    ON user.name=character.name;
```

\newpage
## ANNEXE E - Alternative Datalog Implementation

| name | language |
| :-   |       -: |
| [EvgSkv/logica](https://github.com/EvgSkv/logica)               |  Python |
| [HarvardPLAbcDatalog](https://github.com/HarvardPL/AbcDatalog)  |    Java |
| [c-cube/datalog](https://github.com/c-cube/datalog)             |   Ocaml |
| [catwell/datalog.lua](https://github.com/catwell/datalog.lua)   |     Lua |
| [dave-nachman/datalog](https://github.com/dave-nachman/datalog) |  Python |
| [ekzhang/crepe](https://github.com/ekzhang/crepe)               |    Rust |
| [fogfish/datalog](https://github.com/fogfish/datalog)           |  Erlang |
| [google/mangle](https://github.com/google/mangle)               |      Go |
| [juji-io/datalevin](https://github.com/juji-io/datalevin)       | Clojure |
| [racket/datalog](https://github.com/racket/datalog)             |  Racket |
| [replikativ/datahike](https://github.com/replikativ/datahike)   | Clojure |
| [rust-lang/datafrog](https://github.com/rust-lang/datafrog)     |    Rust |
| [souffle-lang/souffle](https://github.com/souffle-lang/souffle) |     C++ |
| [tonsky/datascript](https://github.com/tonsky/datascript)       | Clojure |
| [travitch/datalog](https://github.com/travitch/datalog)         | Haskell |

\newpage
## ANNEXE F - Unit testing, Dialyzer, and Coverage

Tests and analysis can be executed using make.

```sh
make test
make cover
make dialyzer
```

For the 0.1.0 release, the coverage is around 70%.

|                  module  |    coverage  |
|                        -:|            -:|
|                  `cozo`  |       `76%`  |
|              `cozo_app`  |        `0%`  |
|               `cozo_db`  |       `62%`  |
|              `cozo_nif`  |       `72%`  |
|              `cozo_sup`  |        `0%`  |
|                 `total`  |       `70%`  |

All exported functions are specified and a bunch of types were
created.
