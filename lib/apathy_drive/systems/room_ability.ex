defmodule Systems.RoomAbility do

  import BlockTimer

  def initialize do
    apply_interval 5 |> seconds do
      Components.RoomAbility
      |> Components.all
      |> Enum.each(fn(room) ->
           room
           |> Components.Monsters.get_monsters
           |> Enum.each(fn(monster) ->
                use_ability(monster, Components.RoomAbility.value(room))
              end)
         end)
    end
  end

  def use_ability(monster, ability_name) do
    case Abilities.find(ability_name) do
      nil ->
        Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{ability_name}</span></p>")
      ability ->
        ability.execute(monster, nil)
    end
  end
end