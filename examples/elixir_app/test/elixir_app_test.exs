defmodule ElixirAppTest do
  use ExUnit.Case
  doctest ElixirAppTest

  test "greets the world" do
    assert ElixirAppTest.hello() == :world
  end
end
