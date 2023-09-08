# Cozo Cheatsheet

## Environment Variables

All engines except `mem` one will create a stored database on
filesystem. In case of `sqlite` a single file will be created, and a
directory will be generated with `rocksdb`.

The default path to store the db can be set modified by configuring
`db_path` environment variable. **The directory used must exist**,
cozo will not create it by default.

```erlang
application:set_env(cozo, db_path, "/var/tmp").
{ok
, {_
  , #cozo{
	db_path = "/var/tmp/cozodb_JGsr7QVAREITm25Xq5dAw1lOQovHjvL9"
	}
  }
} = cozo:open().
```

Stored databases are prefixed with `cozodb_` by default, but this
value can be modified by modifying `db_filename_prefix` environment
variable.

```erlang
application:set_env(cozo, db_filename_prefix, "cozodb_custom_prefix_").
{ok
, {_
  , #cozo{
	db_path = "/tmp/cozodb_custom_prefixf5anxr5GZ9CJRhVPR2lLMfSviW4CGUip"
	}
  }
} = cozo:open().
```

`mem` is the default engine used, this value can be changed by
modifying `engine` environment variable.

```erlang
application:set_env(cozo, engine, sqlite).
```

`thoas` is the default JSON parser used to encode and decode JSON
objects. This value can be changed by modiying `json_parser`
environment variable.

```erlang
application:set_env(cozo, json_parser, thoas).
thoas = cozo:json_encoder().
thoas = cozo:json_decoder().
```

Databases can use custom options passed as JSON (or `map()` in this
interface). By default, no options (`#{}`) are passed. This can be
modified by changing `{sqlite, options}` and `{rocksdb, options}`
environment variables.

```erlang
application:set_env(cozo, {sqlite, options}, #{ custom_option => <<>> }).
application:set_env(cozo, {rocksdb, options}, #{ custom_option => <<>> }).
```

The default path to load `cozo_nif.so` file is set to the `priv`
directory linked from this project. This value can be changed by
modifying `lib_path` environment variable.

```erlang
application:set_env(cozo, lib_path, "/usr/lib").
```

## Maintenance Commands

A list of functions have been created to help developers to deal with
maintenance commands:

  - `list_relations/1`
  - `remove_relation/2`
  - `remove_relations/2`
  - `create_relation/3`
  - `replace_relation/3`
  - `put_row/3`
  - `update_row/3`
  - `remove_row/3`
  - `ensure_row/3`
  - `ensure_not_row/3`
  - `list_columns/2`
  - `list_indices/2`
  - `explain/2`
  - `describe/3`
  - `get_triggers/2`
  - `set_access_level/3`
  - `set_access_levels/3`
  - `get_running_queries/1`
  - `kill/2`
  - `compact/1`

## Examples

Test suites in `test` contain the full tutorial from cozodb official
documentation.
