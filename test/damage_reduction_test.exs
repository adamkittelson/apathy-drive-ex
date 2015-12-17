defmodule DamageReductionTest do
  use ExUnit.Case
  use ShouldI
  alias ApathyDrive.Mobile

  having "a Mobile with no physical defense and 1000 magical defense" do
    setup context do
      Dict.put context, :mobile, %Mobile{physical_defense: 0, magical_defense: 1000}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated only by physical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense"]) == 1000
    end

    should("take 560 damage from an ability mitigated only by magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 560
    end

    should("take 560 damage from an ability mitigated only by physical defense and magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense", "magical defense"]) == 560
    end
  end

  having "a Mobile with 500 physical defense and 500 magical defense" do
    setup context do
      Dict.put context, :mobile, %Mobile{physical_defense: 500, magical_defense: 500}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take 780 damage from a 1000 damage ability mitigated only by physical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense"]) == 780
    end

    should("take 780 damage from a 1000 damage ability mitigated only by magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 780
    end

    should("take 608 damage from a 1000 damage ability mitigated by physical defense and magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense", "magical defense"]) == 608
    end
  end

  having "a Mobile with 500 physical defense, 0 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :mobile, %Mobile{physical_defense: 500,
                                         magical_defense:  0,
                                         fire_resistance:  50}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated by magical defense alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 1000
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["fire resistance"]) == 500
    end

    should("take 500 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense", "fire resistance"]) == 500
    end
  end

  having "a Mobile with 500 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :mobile, %Mobile{magical_defense:  500,
                                         fire_resistance:  50}
    end

    should("take 780 damage from an ability mitigated by magical defense alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 780
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["fire resistance"]) == 500
    end

    should("take 390 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense", "fire resistance"]) == 390
    end
  end

end
