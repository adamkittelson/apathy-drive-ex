defmodule UnityDistributionTest do
  use ExUnit.Case, async: true

  setup do
    contributions = %{
      "zero"        => {:mobile, 0},
      "five"        => {:mobile, 5},
      "hundred"     => {:mobile, 100},
      "thousand"    => {:room, 1000},
      "5 thousand"  => {:room, 5000},
      "7 thousand"  => {:room, 7000}
    }

    {:ok, [contributions: contributions]}
  end

  test "correctly calculates essence distribution", context do
    distributions = ApathyDrive.Unity.calculate_distributions(context[:contributions])

    assert distributions["zero"] == {:mobile, 0}
    assert distributions["five"] == {:mobile, 5}
    assert distributions["hundred"] == {:mobile, 100}
    assert distributions["thousand"] == {:room, 1000}
    assert distributions["5 thousand"] == {:room, 447}
    assert distributions["7 thousand"] == {:room, -1552}
    assert 0 ==
      distributions
      |> Map.values
      |> Enum.map(&elem(&1, 1))
      |> Enum.sum
  end
end