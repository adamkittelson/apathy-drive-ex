defmodule MonsterTest do
  use ExUnit.Case
  use ShouldI

  with "an unpossessed monster with no dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{}, spirit: nil}
    end

    should("have 0 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 0
    end
  end

  with "an unpossessed monster with 10 dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"dodge" => 10},
                                           spirit: nil}
    end

    should("have 10 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 10
    end
  end

  with "a monster no dodge possessed by a spirit with 10 power invested in dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{},
                                           spirit: %Spirit{skills: %{"dodge" => 10}}}
    end

    should("have 4 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 4
    end
  end

  with "a monster with 10 dodge possessed by a spirit with 10 power invested in dodge" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"dodge" => 10},
                                           spirit: %Spirit{skills: %{"dodge" => 1000}}}
    end

    should("have 45 dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 45
    end
  end

  with "a monster with 10 dodge possessed by a spirit with 1000 power invested in dodge and 10 dodge from effects" do
    setup context do
      GenServer.start_link(Room, %Room{id: 1, light: 0}, [name: {:global, :room_1}])

      Dict.put context, :monster, %Monster{skills:  %{"dodge" => 10},
                                           effects: %{"some_key" => %{"dodge" => 10}},
                                           spirit:  %Spirit{skills: %{"dodge" => 1000}},
                                           room_id: 1}
    end

    should("have 14 base dodge", context) do
      assert Monster.base_skill(context.monster, "dodge") == 45
    end

    should("have 24 modified dodge", context) do
      assert Monster.modified_skill(context.monster, "dodge") == 55
    end
  end

  with "a monster possessed by a spirit with 1000 devs spent on mage" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{},
                                           spirit: %Spirit{skills: %{"mage" => 1000}}}
    end

    should("have 29 mage", context) do
      assert Monster.base_skill(context.monster, "mage") == 29
    end
  end

  with "a monster with 14 mage" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"mage" => 14}}
    end

    should("have 14 mage", context) do
      assert Monster.base_skill(context.monster, "mage") == 14
    end
  end

  with "a monster with 10 mage possessed by a spirit with 1000 devs spent on mage" do
    setup context do
      Dict.put context, :monster, %Monster{skills: %{"mage" => 10},
                                           spirit: %Spirit{skills: %{"mage" => 1000}}}
    end

    should("have 29 mage", context) do
      assert Monster.base_skill(context.monster, "mage") == 29
    end
  end

end
