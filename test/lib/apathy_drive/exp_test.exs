defmodule ApathyDrive.ExpTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Character

  test "drain rate" do
    assert 1 == trunc(Character.drain_rate(1))
    assert 4 == trunc(Character.drain_rate(10))
    assert 10 == trunc(Character.drain_rate(20))
    assert 22 == trunc(Character.drain_rate(30))
    assert 47 == trunc(Character.drain_rate(40))
    assert 102 == trunc(Character.drain_rate(50))
  end

  test "max exp buffer" do
    assert 333 == Character.max_exp_buffer(%Character{level: 1})
    assert 3471 == Character.max_exp_buffer(%Character{level: 10})
    assert 14608 == Character.max_exp_buffer(%Character{level: 20})
    assert 45451 == Character.max_exp_buffer(%Character{level: 30})
    assert 126_101 == Character.max_exp_buffer(%Character{level: 40})
    assert 330_764 == Character.max_exp_buffer(%Character{level: 50})
  end
end
