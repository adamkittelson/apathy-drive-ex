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
      GenServer.start_link(Room, %Room{id: 1, light: 0}, [name: {:global, :room_1}])

      Dict.put context, :monster, %Monster{skills:  %{"dodge" => %{"base" => 10, "trained" => 10}},
                                           effects: %{"some_key" => %{"dodge" => 10}},
                                           strength: 0,
                                           agility:  0,
                                           intelligence: 0,
                                           health: 0,
                                           room_id: 1}
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

  with "100 devs spent on armour" do
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
  end

  with "a monster with 14 armour" do
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
  end

  with "a monster with 10 armour" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"armour" => %{"base" => 10, "trained" => 100}}}
    end

    should("have trained armour", context) do
      assert Monster.trained_skills(context.monster) == ["armour"]
    end

    should("have 24 armour", context) do
      assert Monster.base_skill(context.monster, "armour") == 24
    end
  end

end
