defmodule ApathyDrive.ExpTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Character

  test "drain rate" do
    assert 1 == trunc(Character.drain_rate(1))
    assert 2 == trunc(Character.drain_rate(10))
    assert 5 == trunc(Character.drain_rate(20))
    assert 11 == trunc(Character.drain_rate(30))
    assert 23 == trunc(Character.drain_rate(40))
    assert 51 == trunc(Character.drain_rate(50))
  end
end
