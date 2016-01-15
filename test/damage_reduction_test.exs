defmodule DamageReductionTest do
  use ExUnit.Case
  use ShouldI
  alias ApathyDrive.Mobile

  having "a Mobile with no physical defense and 1000 magical defense" do
    setup context do
      Dict.put context, :mobile, %Mobile{strength: 1, agility: 249, will: 249}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated only by physical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense"]) == 999
    end

    should("take 560 damage from an ability mitigated only by magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 560
    end

    should("take 560 damage from an ability mitigated only by physical defense and magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense", "magical defense"]) == 559
    end
  end

  having "a Mobile with 500 physical defense and 500 magical defense" do
    setup context do
      Dict.put context, :mobile, %Mobile{strength: 165, agility: 165, will: 165}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take 780 damage from a 1000 damage ability mitigated only by physical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense"]) == 781
    end

    should("take 780 damage from a 1000 damage ability mitigated only by magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 781
    end

    should("take 608 damage from a 1000 damage ability mitigated by physical defense and magical defense", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["physical defense", "magical defense"]) == 610
    end
  end

  having "a Mobile with 500 physical defense, 0 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :mobile, %Mobile{strength: 165, agility: 165, will: 1, effects: %{"test" => %{"fire resistance" => 50}}}
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated by magical defense alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 999
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["fire resistance"]) == 500
    end

    should("take 500 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense", "fire resistance"]) == 499
    end
  end

  having "a Mobile with 500 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :mobile, %Mobile{strength: 165, agility: 165, will: 165, effects: %{"test" => %{"fire resistance" => 50}}}
    end

    should("take 780 damage from an ability mitigated by magical defense alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 781
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["fire resistance"]) == 500
    end

    should("take 390 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense", "fire resistance"]) == 390
    end
  end

  having "a Mobile with 500 magical defense and 50 fire resistance and 50 damage resistance" do
    setup context do
      Dict.put context, :mobile, %Mobile{strength: 165, agility: 165, will: 165, effects: %{"test" => %{"fire resistance" => 50,
                                                                "damage resistance" => 50}}}
    end

    should("take 500 damage from an ability mitigated by nothing", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, []) == 500
    end
    
    should("take 390 damage from an ability mitigated by magical defense alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense"]) == 390
    end

    should("take 250 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["fire resistance"]) == 250
    end

    should("take 195 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Mobile.reduce_damage(context.mobile, 1000, ["magical defense", "fire resistance"]) == 195
    end
  end

end
