defmodule MonsterTest do
  use ExUnit.Case
  use ShouldI

  # with "an unpossessed monster with no dodge" do
  #   setup context do
  #     Dict.put context, :monster, %Monster{skills: %{}, spirit: nil}
  #   end
  # 
  #   should("have 0 dodge", context) do
  #     assert Monster.base_skill(context.monster, "dodge") == 0
  #   end
  # end
  # 
  # with "an unpossessed monster with 10 dodge" do
  #   setup context do
  #     Dict.put context, :monster, %Monster{skills: %{"dodge" => 10},
  #                                          spirit: nil}
  #   end
  # 
  #   should("have 10 dodge", context) do
  #     assert Monster.base_skill(context.monster, "dodge") == 10
  #   end
  # end
  # 
  # with "a monster with 10 dodge with 10 dodge from effects" do
  #   setup context do
  #     GenServer.start_link(Room, %Room{id: 1, light: 0}, [name: {:global, :room_1}])
  # 
  #     Dict.put context, :monster, %Monster{skills:  %{"dodge" => 10},
  #                                          effects: %{"some_key" => %{"dodge" => 10}},
  #                                          room_id: 1}
  #   end
  # 
  #   should("have 10 base dodge", context) do
  #     assert Monster.base_skill(context.monster, "dodge") == 10
  #   end
  # 
  #   should("have 24 modified dodge", context) do
  #     assert Monster.modified_skill(context.monster, "dodge") == 20
  #   end
  # end

end
