defmodule Systems.Exits.Item do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit) do
    cond do
      Systems.Combat.stunned?(monster) ->
        send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
      Systems.Item.has_item?(monster, room_exit["item"]) ->
        item = monster
              |> Components.Items.get_items
              |> Enum.find(fn(item) ->
                   Components.Name.value(item) == room_exit["item"]
                 end)

        destination = Rooms.find_by_id(room_exit["destination"])
        Components.Monsters.remove_monster(current_room, monster)
        Components.Monsters.add_monster(destination, monster)
        Entities.save!(destination)
        Entities.save!(current_room)
        Components.Uses.use!(item, monster)
        Entities.save(monster)

        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Aggression.monster_entered(monster, destination)

        Monster.pursue(current_room, monster, room_exit["direction"])
      true ->
        send_message(monster, "scroll", "<p><span class='yellow'>#{room_exit["failure_message"]}</span></p>")
    end
  end

  def move(spirit, monster, current_room, room_exit) do
    cond do
      Systems.Combat.stunned?(monster) ->
        send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
      Systems.Item.has_item?(monster, room_exit["item"]) ->
        item = monster
              |> Components.Items.get_items
              |> Enum.find(fn(item) ->
                   Components.Name.value(item) == room_exit["item"]
                 end)

        destination = Rooms.find_by_id(room_exit["destination"])
        Components.Monsters.remove_monster(current_room, monster)
        Components.Monsters.add_monster(destination, monster)
        Components.Characters.remove_character(current_room, spirit)
        Components.Characters.add_character(destination, spirit)
        Entities.save!(destination)
        Entities.save!(current_room)
        send_message(monster, "scroll", "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
        Components.Uses.use!(item, monster)
        Entities.save!(spirit)
        Entities.save(monster)

        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)

        Systems.Aggression.monster_entered(monster, destination)

        Components.Hints.deactivate(spirit, "movement")
        Systems.Room.display_room_in_scroll(monster, destination)
        Monster.pursue(current_room, monster, room_exit["direction"])
      true ->
        send_message(monster, "scroll", "<p><span class='yellow'>#{room_exit["failure_message"]}</span></p>")
    end
  end

end
