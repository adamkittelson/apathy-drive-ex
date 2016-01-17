defmodule UnityDistributionTest do
  use ExUnit.Case, async: true

  setup do
    contributions = %{
      "zero"        => 0,
      "five"        => 5,
      "hundred"     => 100,
      "thousand"    => 1000,
      "5 thousand"  => 5000,
      "7 thousand"  => 7000
    }

    {:ok, [contributions: contributions]}
  end

  test "correctly calculates essence distribution", context do
    distributions = ApathyDrive.Unity.calculate_distributions(context[:contributions])

    assert distributions["zero"] == 0
    assert distributions["five"] == 5
    assert distributions["hundred"] == 100
    assert distributions["thousand"] == 1000
    assert distributions["5 thousand"] == 447
    assert distributions["7 thousand"] == -1552
    assert Enum.sum(Map.values(distributions)) == 0
  end
end