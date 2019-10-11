defmodule ApathyDrive.ExpTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Character

  test "drain rate" do
    assert 10 == Character.drain_rate(%Character{level: 1})
    assert 21 == Character.drain_rate(%Character{level: 10})
    assert 51 == Character.drain_rate(%Character{level: 20})
    assert 111 == Character.drain_rate(%Character{level: 30})
    assert 239 == Character.drain_rate(%Character{level: 40})
    assert 510 == Character.drain_rate(%Character{level: 50})
  end

  test "max exp buffer" do
    assert 3600 == Character.max_exp_buffer(%Character{level: 1})
    assert 7560 == Character.max_exp_buffer(%Character{level: 10})
    assert 18360 == Character.max_exp_buffer(%Character{level: 20})
    assert 39960 == Character.max_exp_buffer(%Character{level: 30})
    assert 86040 == Character.max_exp_buffer(%Character{level: 40})
    assert 183_600 == Character.max_exp_buffer(%Character{level: 50})
  end
end
