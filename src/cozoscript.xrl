%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc Cozoscript lexer definition and rules.
%%%
%%% == Notes ==
%%%
%%% The work on this parser started on 2023-09-09. The goal is to
%%% create a way to valid cozoscript directly on the Erlang side, and
%%% possibly convert it in another language (or at least having a
%%% working AST to play with). Cozoscript syntax and grammar is
%%% defined in `cozoscript.pest' file[1]. In fact, this is probably
%%% the easiest way to control and validate user input or our own
%%% queries.
%%%
%%% The second goal is to convert Erlang expressions into cozoscript
%%% bytecode defined in `expr.rs' file [2].  Unfortunately, this task
%%% is quite complex for the moment. The operators definition can be
%%% found in `functions.rs' file [3]. Each operators are defined as
%%% function with the help of a macro called `define_op' [4].
%%%
%%% Converting an Erlang code like into a bytecode is an hard task,
%%% and before doing that, working on the syntax we want is probably
%%% what we are looking for. Let start with a simple example using
%%% cozoscript. The following code can be used directly in your
%%% favorite browser with Cozoscript demo [DEMO].
%%%
%%% ```
%%% ?[] <- [[1,2,3]]
%%% '''
%%%
%%% This code simply returns a column similar to the following one
%%% (directly from WASM code).
%%%
%%% ```
%%% Line | _0 | _1 | _2 |
%%%    1 |  1 |  2 |  3 |
%%% '''
%%%
%%% What could be the equivalent code using Erlang syntax? Do we have
%%% something similar? I think we have Match Specifications [MATCH]
%%% from ETS [ETS] and Mnesia [MNESIA] to deal with that, and can be
%%% plugged with `qlc' [QLC] module. Comprehension syntax is also
%%% close to this example.
%%%
%%% ```
%%% [1,2,3] = [ X || X <- [1,2,3] ].
%%% '''
%%%
%%% Defining a record containg the columns (field names) and their
%%% values.
%%%
%%% ```
%%% #relation{ c1 = "", c2 = "" }.
%%% '''
%%%
%%% This implementation is using `leex' and `yecc'. More `leex'
%%% examples can be found in `leex_SUITE.erl' file [LEEX].
%%%
%%% [1] https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest
%%% [2] https://github.com/cozodb/cozo/blob/main/cozo-core/src/data/expr.rs
%%% [3] https://github.com/cozodb/cozo/blob/main/cozo-core/src/data/functions.rs
%%% [4] https://github.com/cozodb/cozo/blob/main/cozo-core/src/data/functions.rs#L37
%%% [DEMO] https://www.cozodb.org/wasm-demo/
%%% [ETS] https://www.erlang.org/doc/man/ets.html#
%%% [LEEX] lib/parsetools/test/leex_SUITE.erl
%%% [MATCH] https://www.erlang.org/doc/apps/erts/match_spec.html
%%% [QLC] https://www.erlang.org/doc/man/qlc.html#
%%% [MNESIA] https://www.erlang.org/doc/man/mnesia.html#
%%%
%%% @end
%%%===================================================================

Definitions.

Comments = \#.*
Commands = [a-z_]+
WS = ([\000-\s]|#.*)

Rules.

\|\|  : {token, {op_or, TokenLine, TokenChars}}.
\&\&  : {token, {op_and, TokenLine, TokenChars }}.
\+\+  : {token, {op_concat, TokenLine, TokenChars }}.
\+    : {token, {op_add, TokenLine, TokenChars }}.
->    : {token, {op_field_access, TokenLine, TokenChars }}.
-     : {token, {op_sub, TokenLine, TokenChars }}.
\*    : {token, {op_mul, TokenLine, TokenChars }}.
/     : {token, {op_div, TokenLine, TokenChars }}.
\%    : {token, {op_mod, TokenLine, TokenChars }}.
==    : {token, {op_eq, TokenLine, TokenChars }}.
!=    : {token, {op_ne, TokenLine, TokenChars }}.
>     : {token, {op_gt, TokenLine, TokenChars }}.
<     : {token, {op_lt, TokenLine, TokenChars }}.
>=    : {token, {op_ge, TokenLine, TokenChars }}.
<=    : {token, {op_le, TokenLine, TokenChars }}.
\^    : {token, {op_pow, TokenLine, TokenChars }}.
~     : {token, {op_coalesce, TokenLine, TokenChars }}.
% {.*}- : {token, {op_minus, TokenLine, TokenChars }}.
% {.*}! : {token, {op_negate, TokenLine, TokenChars }}.

\:\:{Commands}  : {token, {command(access_level), TokenLine, TokenChars}}.
{SysScriptCommands}  : {token,{sys_script,TokenLine,TokenChars}}.
{WS}+ : skip_token.

Erlang code.
