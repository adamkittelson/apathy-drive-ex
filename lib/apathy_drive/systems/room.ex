defmodule Systems.Room do
  use Systems.Reload
  import Utility

  # def long_room_html(spirit, monster, room) do
  #   light = light_level(monster)
  #   if light > -200 and light < 200 do
  #     directions = Room.exit_directions(room) |> exit_directions_html
  #     "<div class='room'>#{name_html(room)}#{description_html(room)}#{shop(room)}#{items_html(room)}#{monsters_html(monster, room)}#{directions}#{light(monster)}</div>"
  #   else
  #     "<div class='room'>#{light(monster)}</div>"
  #   end
  # end
  # 
  # def light_desc(light_level)  when light_level < -1000, do: "<p>You are blind.</p>"
  # def light_desc(light_level)  when light_level <= -300, do: "<p>The room is pitch black - you can't see anything</p>"
  # def light_desc(light_level)  when light_level <= -200, do: "<p>The room is very dark - you can't see anything</p>"
  # def light_desc(light_level)  when light_level <= -100, do: "<p>The room is barely visible</p>"
  # def light_desc(light_level)  when light_level <=  -25, do: "<p>The room is dimly lit</p>"
  # def light_desc(light_level)  when light_level >=  300, do: "<p>The room is blindingly bright - you can't see anything</p>"
  # def light_desc(light_level)  when light_level >=  200, do: "<p>The room is painfully bright - you can't see anything</p>"
  # def light_desc(light_level)  when light_level >=  100, do: "<p>The room is dazzlingly bright</p>"
  # def light_desc(light_level)  when light_level >=   25, do: "<p>The room is brightly lit</p>"
  # def light_desc(light_level), do: nil
  # 
  # def light(monster) do
  #   light_level(monster)
  #   |> light_desc
  # end
  # 
  # def light_level(%{room_id: nil}, monster), do: 0
  # def light_level(monster) do
  #   room = Monster.room(monster)
  #   alignment = Components.Alignment.value(monster)
  #   light     = room.light + light_in_room(room) + light_on_monsters(room)
  # 
  #   cond do
  #     alignment > 0 and light < 0 ->
  #       min(0, light + alignment)
  #     alignment < 0 and light > 0 ->
  #       max(0, light + alignment)
  #     true ->
  #       light
  #   end
  #   
  # end
  # 
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
  # 
  # def entities(entity, room) do
  #   Room.monsters(room) |> Enum.reject(&(entity == &1))
  # end
  # 
  # def move(spirit, monster, direction) do
  #   ApathyDrive.Exit.move(spirit, monster, direction)
  # end

end
