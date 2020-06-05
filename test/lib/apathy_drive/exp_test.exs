defmodule ApathyDrive.ExpTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Character

  test "drain rate" do
    assert 1 == Character.drain_rate(%Character{level: 1})
    assert 2 == Character.drain_rate(%Character{level: 10})
    assert 5 == Character.drain_rate(%Character{level: 20})
    assert 11 == Character.drain_rate(%Character{level: 30})
    assert 24 == Character.drain_rate(%Character{level: 40})
    assert 51 == Character.drain_rate(%Character{level: 50})
  end

  test "max exp buffer" do
    assert 3600 == Character.max_exp_buffer(%Character{level: 1})
    assert 7200 == Character.max_exp_buffer(%Character{level: 10})
    assert 18000 == Character.max_exp_buffer(%Character{level: 20})
    assert 39600 == Character.max_exp_buffer(%Character{level: 30})
    assert 86400 == Character.max_exp_buffer(%Character{level: 40})
    assert 183_600 == Character.max_exp_buffer(%Character{level: 50})
  end
end
