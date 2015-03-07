defmodule ApathyDrive.Exits.Item do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)
  def move(nil, monster, current_room, room_exit) do
    cond do
      Systems.Item.has_item?(monster, room_exit["item"]) ->
        item = monster
              |> Components.Items.get_items
              |> Enum.find(fn(item) ->
                   Components.Name.value(item) == room_exit["item"]
                 end)

        destination = Room.find(room_exit["destination"])
        Components.Monsters.remove_monster(current_room, monster)
        Components.Monsters.add_monster(destination, monster)
        if Entity.has_component?(monster, Components.ID) do
          Entities.save!(destination)
          Entities.save!(current_room)
        end
        Components.Uses.use!(item)
        Entities.save(monster)

        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Aggression.monster_entered(monster, destination)

        Monster.pursue(current_room, monster, room_exit["direction"])
      true ->
        Monster.send_scroll(monster, "<p><span class='yellow'>#{room_exit["failure_message"]}</span></p>")
    end
  end

  def move(spirit, monster, current_room, room_exit) do
    cond do
      Systems.Item.has_item?(monster, room_exit["item"]) ->
        item = monster
              |> Components.Items.get_items
              |> Enum.find(fn(item) ->
                   Components.Name.value(item) == room_exit["item"]
                 end)

        destination = Room.find(room_exit["destination"])
        Components.Monsters.remove_monster(current_room, monster)
        Components.Monsters.add_monster(destination, monster)
        Components.Characters.remove_character(current_room, spirit)
        Components.Characters.add_character(destination, spirit)
        Entities.save!(destination)
        Entities.save!(current_room)
        Monster.send_scroll(monster, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
        Components.Uses.use!(item)
        Entities.save!(spirit)
        Entities.save(monster)

        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Aggression.monster_entered(monster, destination)

        Spirit.deactivate_hint(spirit, "movement")
        Systems.Room.display_room_in_scroll(spirit, monster, destination)
        Monster.pursue(current_room, monster, room_exit["direction"])
      true ->
        Monster.send_scroll(monster, "<p><span class='yellow'>#{room_exit["failure_message"]}</span></p>")
    end
  end

end
