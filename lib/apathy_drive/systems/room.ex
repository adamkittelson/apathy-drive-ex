defmodule Systems.Room do
  use Systems.Reload
  import Utility

  # def lights(items) do
  #   items
  #   |> Enum.filter(&(Entity.has_component?(&1, Components.Effects)))
  #   |> Enum.map(fn(item) ->
  #        item
  #        |> Components.Effects.lights
  #        |> Enum.sum
  #      end)
  #   |> Enum.sum
  # end
  # 
  # def light_in_room(room) do
  #   Room.items(room)
  #   |> lights
  # end
  # 
  # def light_on_monsters(room) do
  #   room
  #   |> Systems.Room.living_in_room
  #   |> Enum.map(fn(monster) ->
  #        Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster)
  #      end)
  #   |> List.flatten
  #   |> lights
  # end

end
