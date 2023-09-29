---
status: draft
date: 2023-09-29
title: CozoDB Erlang Implementation
subtitle: From C NIF to Rust with Rustler
author: Mathieu Kerjouan
keywords: [cozo,cozodb,erlang,otp,datalog,prolog,relation,database,rust,rustler]
license: CC BY-NC-ND
abstract:
---

The author of this article never created any application with Rust. An
introduction to Rust development and how to integrate it with Erlang
can be found in _Annexes_. If Rust code looks weird in the following
snippets, don't hesitate to correct them or explain why they are
incorrect.

# CozoDB Erlang Implementation

## From C NIF to Rust with Rustler

# ANNEXE A - 101 Introduction to Rust



Thanks to the huge Rust community and all the resources, it was not so
_hard_ to learn it, but this language is way more complex than C or
Erlang.

# ANNEXE B - Using Rust, Rustler and Erlang



[^rustler]: https://github.com/rusterlium/rustler
[^rust-create-rustler]: https://docs.rs/rustler/latest/rustler
[^rustler::Term]: https://docs.rs/rustler/latest/rustler/struct.Term.html
[^rustler::TermType]: https://docs.rs/rustler/latest/rustler/enum.TermType.html
[^rebar3_cargo]: https://github.com/rusterlium/rebar3_cargo
