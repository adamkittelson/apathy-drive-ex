defmodule Systems.RoomAbility do
  use Systems.Reload
  import Utility
  import BlockTimer
  import Systems.Text

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
        send_message(monster, "scroll", "<p><span class='red'>Not Implmented: #{ability_name}</span></p>")
      ability ->
        ability.execute(monster)
    end
  end
end