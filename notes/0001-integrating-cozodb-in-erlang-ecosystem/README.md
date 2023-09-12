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

 - [ ] introduction Datalog
 - [ ] introducing Cozodb
 - [ ] introducing NIF

## A Quick Overview of CozoDB

 - [ ] using CozoDB REPL
 - [ ] describe and use Cozoscript syntax

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

## Erlang Interface to NIF

 - [ ] describe `cozo` module
 - [ ] describe `cozo_db` module

## Using CozoDB with Erlang

 - [ ] usage example with `cozo` module
 - [ ] usage example with `cozo_db` module

## Future Improvements

 - [ ] cozodb interface with rustler
 - [ ] erlang terms to cozodb bytecode

## Conclusion

 - [ ] issues with library path

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
