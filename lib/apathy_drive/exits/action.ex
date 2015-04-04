defmodule ApathyDrive.Exits.Action do
  use ApathyDrive.Exit

  def move(_current_room, %Spirit{} = spirit, room_exit) do
    new_room = Room.find(room_exit["destination"])
               |> Room.value

    Spirit.send_scroll(spirit, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => spirit})}</span></p>")

    Room.look(new_room, spirit)

    spirit
    |> Spirit.set_room_id(room_exit["destination"])
    |> Spirit.deactivate_hint("movement")
    |> Spirit.save
  end

  def move(%Room{} = room, %Monster{} = monster, room_exit) do
    destination = Room.find(room_exit["destination"])
                  |> Room.value

    Room.send_scroll(destination, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")

    monster = monster
              |> Monster.set_room_id(room_exit["destination"])
              |> Monster.save

    Monster.send_scroll(monster, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")

    Room.look(destination, monster)

    Room.send_scroll(room, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
    monster
  end

end
