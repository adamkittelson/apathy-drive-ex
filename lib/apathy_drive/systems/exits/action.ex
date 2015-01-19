defmodule Systems.Exits.Action do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit), do: super(nil, monster, current_room, room_exit)

  def move(spirit, monster, current_room, room_exit) do
    if Systems.Combat.stunned?(monster) do
      send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
    else
      destination = Room.find(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      Components.Characters.remove_character(current_room, spirit)
      Components.Characters.add_character(destination, spirit)
      Entities.save!(destination)
      Entities.save!(current_room)
      send_message(monster, "scroll", "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
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

      Spirit.deactivate_hint(spirit, "movement")
      Systems.Room.display_room_in_scroll(spirit, monster, destination)
      Monster.pursue(current_room, monster, room_exit["direction"])
    end
  end

end
