defmodule ApathyDrive.Exits.Command do
  use ApathyDrive.Exit

  def display_direction(_room, room_exit) do
    room_exit["name"]
  end

  def move(_current_room, %Spirit{} = spirit, _room_exit)  do
    Spirit.send_scroll(spirit, "<p>There is no exit in that direction.</p>")
  end

  def move(_current_room, %Monster{} = monster, _room_exit) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
  end

  def move_via_command(_current_room, %Spirit{} = spirit, room_exit) do
    Spirit.send_scroll(spirit, "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>")

    new_room = Room.find(room_exit["destination"])
               |> Room.value

    Room.look(new_room, spirit)

    spirit
    |> Spirit.set_room_id(room_exit["destination"])
    |> Spirit.deactivate_hint("movement")
    |> Spirit.save
  end

  def move_via_command(%Room{} = room, %Monster{} = monster, room_exit) do
    destination = Room.find(room_exit["destination"])
                  |> Room.value

    if room_exit["to_message"] do
      Room.send_scroll(destination, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
    else
      notify_monster_entered(monster, room, destination)
    end

    monster = monster
              |> Monster.set_room_id(room_exit["destination"])
              |> Monster.save

    Monster.send_scroll(monster, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")

    Room.look(destination, monster)

    if room_exit["from_message"] do
      Room.send_scroll(room, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
    else
      notify_monster_left(monster, room, destination)
    end
    monster
  end

  def look(%Room{}, %Spirit{} = spirit, _room_exit) do
    Spirit.send_scroll(spirit, "<p>There is no exit in that direction.</p>")
  end

  def look(%Room{}, %Monster{} = monster, _room_exit) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
  end

end
