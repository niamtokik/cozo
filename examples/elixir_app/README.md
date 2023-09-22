# ElixirApp

An example demonstrating how to add Cozo to an elixir application.

## Checking it runs

Open a terminal on the directory this file is located.

Then as Elixir `mix` to get the dependencies and compile.

```shell
mix deps.get
mix compile
```

The run the elixir shell.

```shell
iex  -S mix
```

Once you are in the shell type:

```elixir
{:ok, db} = :cozo.open()
```

You should get a CozoDB reference


