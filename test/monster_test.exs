defmodule MonsterTest do
  use ExUnit.Case
  use ShouldI

  with "a monster with no base dodge and no trained dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{}}
    end

    should("have 0 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 0
    end

    should("have no trained skills", context) do
      assert Monster.trained_skills(context.monster) == []
    end
  end

  with "a monster with 10 base dodge and no trained dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"dodge" => %{"base" => 10}}}
    end

    should("have no trained skills", context) do
      assert Monster.trained_skills(context.monster) == []
    end

    should("have 10 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 10
    end
  end

  with "a monster with no base dodge and 10 power invested in dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"dodge" => %{"trained" => 10}}}
    end

    should("have trained dodge", context) do
      assert Monster.trained_skills(context.monster) == ["dodge"]
    end

    should("have 4 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 4
    end
  end

  with "a monster with 10 base dodge and 10 power invested in dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"dodge" => %{"base" => 10, "trained" => 10}}}
    end

    should("have trained dodge", context) do
      assert Monster.trained_skills(context.monster) == ["dodge"]
    end

    should("have 14 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 14
    end
  end

  with "a monster with 10 base dodge and 10 power invested in dodge and 10 dodge from effects" do
    setup context do
      Dict.put context, :monster, %Monster{skills:  %{"dodge" => %{"base" => 10, "trained" => 10}},
                                           effects: %{"some_key" => %{"dodge" => 10}},
                                           strength: 0,
                                           agility:  0,
                                           intelligence: 0,
                                           health: 0}
    end

    should("have trained dodge", context) do
      assert Monster.trained_skills(context.monster) == ["dodge"]
    end

    should("have 14 base dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 14
    end

    should("have 24 modified dodge", context) do
      assert Monster.modified_skill(context.monster, "dodge") == 24
    end
  end

  with "a monster with 10 strength and no skills" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 10,
                                           skills: %{}}
    end

    should("have no trained skills", context) do
      assert Monster.trained_skills(context.monster) == []
    end

    should("have 10 strength", context) do
      assert Monster.modified_stat(context.monster, "strength") == 10
    end
  end

  with "a monster with 10 strength and 100 devs spent on armour" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 10,
                                           skills: %{"armour" => %{"trained" => 100}}}
    end

    should("have trained armour", context) do
      assert Monster.trained_skills(context.monster) == ["armour"]
    end

    should("have 14 armour", context) do
      assert Monster.base_skill(context.monster, "armour") == 14
    end

    should("have 17 strength", context) do
      assert Monster.modified_stat(context.monster, "strength") == 17
    end
  end

  with "a monster with 10 strength and 14 armour" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 10,
                                           skills: %{"armour" => %{"base" => 14}}}
    end

    should("have no trained skills", context) do
      assert Monster.trained_skills(context.monster) == []
    end

    should("have 14 armour", context) do
      assert Monster.base_skill(context.monster, "armour") == 14
    end

    should("have 10 strength", context) do
      assert Monster.modified_stat(context.monster, "strength") == 10
    end
  end

  with "a monster with 10 strength and 5 strength from effects" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 10,
                                           skills: %{},
                                           effects: %{"some_key" => %{"strength" => 2},
                                                      "some_other_key" => %{"strength" => 3}}}
    end

    should("have no trained skills", context) do
      assert Monster.trained_skills(context.monster) == []
    end

    should("have 15 strength", context) do
      assert Monster.modified_stat(context.monster, "strength") == 15
    end
  end

  with "a monster with 10 strength and 10 armour, and 5 strength from effects" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 10,
                                           skills: %{"armour" => %{"base" => 10, "trained" => 100}},
                                           effects: %{"some_key" => %{"strength" => 2},
                                                      "some_other_key" => %{"strength" => 3}}}
    end

    should("have trained armour", context) do
      assert Monster.trained_skills(context.monster) == ["armour"]
    end

    should("have 24 armour", context) do
      assert Monster.base_skill(context.monster, "armour") == 24
    end

    should("have 22 strength", context) do
      assert Monster.modified_stat(context.monster, "strength") == 22
    end
  end

  with "a monster with 3 strength and 3 health" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 3, health: 3}
    end

    should("have 33 max hp", context) do
      assert Monster.max_hp(context.monster) == 33
    end
  end

  with "a monster with 50 strength and 100 health" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 50, health: 100}
    end

    should("have 1601 max hp", context) do
      assert Monster.max_hp(context.monster) == 1601
    end
  end

  with "a monster with 100 strength and 50 health" do
    setup context do
      Dict.put context, :monster, %Monster{strength: 100, health: 50}
    end

    should("have 1161 max hp", context) do
      assert Monster.max_hp(context.monster) == 1161
    end
  end

  with "a monster with intelligence" do
    setup context do
      Dict.put context, :monster, %Monster{intelligence: 3}
    end

    should("have 1 max mana", context) do
      assert Monster.max_mana(context.monster) == 1
    end
  end

  with "a monster with 50 intelligence" do
    setup context do
      Dict.put context, :monster, %Monster{intelligence: 50}
    end

    should("have 40 max mana", context) do
      assert Monster.max_mana(context.monster) == 40
    end
  end

  with "a monster with 100 intelligence" do
    setup context do
      Dict.put context, :monster, %Monster{intelligence: 100}
    end

    should("have 83 max mana", context) do
      assert Monster.max_mana(context.monster) == 83
    end
  end

end
