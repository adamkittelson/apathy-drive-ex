defmodule ApathyDrive.Exits.Action do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit) do
    new_room = Room.find(room_exit["destination"])
               |> Room.value

    Spirit.send_scroll(spirit, "<p><span class='yellow'>#{interpolate(room_exit.mover_message, %{"user" => spirit})}</span></p>")

    Room.look(new_room, spirit)

    spirit
    |> Spirit.set_room_id(room_exit["destination"])
    |> Spirit.deactivate_hint("movement")
    |> Spirit.save
  end

  def move(current_room, %Monster{} = monster, room_exit) do
    if Systems.Combat.stunned?(monster) do
      send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
    else
      destination = Room.find(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      Entities.save!(destination)
      Entities.save!(current_room)
      send_message(monster, "scroll", "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
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

      #Systems.Room.display_room_in_scroll(spirit, monster, destination)
      Monster.pursue(current_room, monster, room_exit["direction"])
    end
  end

end
