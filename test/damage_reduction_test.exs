defmodule DamageReductionTest do
  use ExUnit.Case
  use ShouldI
  alias ApathyDrive.Monster

  having "a Monster with no physical defense and 1000 magical defense" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 0, agility: 0, will: 1248}
    end

    should("have 0 physical defense", context) do
      assert trunc(Monster.physical_defense(context.monster)) == 0
    end

    should("have 1000 magical defense", context) do
      assert trunc(Monster.magical_defense(context.monster)) == 1000
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Monster.reduce_damage(context.monster, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Monster.reduce_damage(context.monster, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated only by physical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["physical defense"]) == 1000
    end

    should("take 559 damage from an ability mitigated only by magical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense"]) == 559
    end

    should("take 559 damage from an ability mitigated only by physical defense and magical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["physical defense", "magical defense"]) == 559
    end
  end

  having "a Monster with 500 physical defense and 500 magical defense" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 449, agility: 349, will: 449}
    end

    should("have 500 physical defense", context) do
      assert trunc(Monster.physical_defense(context.monster)) == 500
    end

    should("have 500 magical defense", context) do
      assert trunc(Monster.magical_defense(context.monster)) == 500
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Monster.reduce_damage(context.monster, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Monster.reduce_damage(context.monster, 1000, nil) == 1000
    end

    should("take 779 damage from a 1000 damage ability mitigated only by physical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["physical defense"]) == 779
    end

    should("take 779 damage from a 1000 damage ability mitigated only by magical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense"]) == 779
    end

    should("take 608 damage from a 1000 damage ability mitigated by physical defense and magical defense", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["physical defense", "magical defense"]) == 608
    end
  end

  having "a Monster with 500 physical defense, 0 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 624, agility: 0, will: 0, effects: %{"test" => %{"fire resistance" => 50}}}
    end

    should("have 500 physical defense", context) do
      assert trunc(Monster.physical_defense(context.monster)) == 500
    end

    should("have 0 magical defense", context) do
      assert trunc(Monster.magical_defense(context.monster)) == 0
    end

    should("take full damage from an ability with no mitigations", context) do
      assert Monster.reduce_damage(context.monster, 1000, []) == 1000
    end

    should("take full damage from an ability no mitigations specified", context) do
      assert Monster.reduce_damage(context.monster, 1000, nil) == 1000
    end

    should("take full damage from an ability mitigated by magical defense alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense"]) == 1000
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["fire resistance"]) == 500
    end

    should("take 500 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense", "fire resistance"]) == 500
    end
  end

  having "a Monster with 500 magical defense and 50 fire resistance" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 0, agility: 349, will: 449, effects: %{"test" => %{"fire resistance" => 50}}}
    end

    should("have 500 magical defense", context) do
      assert trunc(Monster.magical_defense(context.monster)) == 500
    end

    should("take 779 damage from an ability mitigated by magical defense alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense"]) == 779
    end

    should("take 500 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["fire resistance"]) == 500
    end

    should("take 389 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense", "fire resistance"]) == 389
    end
  end

  having "a Monster with 500 magical defense and 50 fire resistance and 50 damage resistance" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 0, agility: 349, will: 449, effects: %{"test" => %{"fire resistance" => 50,
                                                                "damage resistance" => 50}}}
    end

    should("have 500 magical defense", context) do
      assert trunc(Monster.magical_defense(context.monster)) == 500
    end

    should("take 500 damage from an ability mitigated by nothing", context) do
      assert Monster.reduce_damage(context.monster, 1000, []) == 500
    end

    should("take 389 damage from an ability mitigated by magical defense alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense"]) == 389
    end

    should("take 250 damage from a 1000 damage ability mitigated only by fire resistance alone", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["fire resistance"]) == 250
    end

    should("take 194 damage from a 1000 damage ability mitigated by magical defense and fire resistance", context) do
      assert Monster.reduce_damage(context.monster, 1000, ["magical defense", "fire resistance"]) == 194
    end
  end

end
