%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @doc Cozoscript lexer definition and rules.
%%%
%%% see: https://github.com/cozodb/cozo/blob/main/cozo-core/src/cozoscript.pest
%%% see: lib/parsetools/test/leex_SUITE.erl
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
