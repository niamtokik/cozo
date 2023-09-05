# cozo

A quick and dirty NIF interface to cozodb.

## Todo

This is just a PoC... Lot of things to do.

 - [ ] create test suite
 - [ ] and much more...

## Build

```sh
rebar3 compile
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

// close the database
ok = cozo:close(Db).
```
