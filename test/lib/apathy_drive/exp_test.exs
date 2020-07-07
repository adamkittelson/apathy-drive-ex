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

  test "max exp buffer" do
    assert 300 == Character.max_exp_buffer(%Character{level: 1})
    assert 1735 == Character.max_exp_buffer(%Character{level: 10})
    assert 7304 == Character.max_exp_buffer(%Character{level: 20})
    assert 22725 == Character.max_exp_buffer(%Character{level: 30})
    assert 63050 == Character.max_exp_buffer(%Character{level: 40})
    assert 165_382 == Character.max_exp_buffer(%Character{level: 50})
  end
end
