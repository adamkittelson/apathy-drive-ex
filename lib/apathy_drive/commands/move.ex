defmodule ApathyDrive.Commands.Move do
  alias ApathyDrive.{Doors, Monster, Room, RoomServer}

  def execute(%Room{} = room, %Monster{} = monster, command) when is_binary(command) do
    direction = Room.direction(command)
    room_exit = Room.get_exit(room, direction)
    execute(room, monster, room_exit)
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => kind} = room_exit) when kind in ["Trap", "Cast"] do
    execute(room, monster, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"] do
    Monster.send_scroll(monster, "<p><span class='dark-green'>You pass right through the #{String.downcase(kind)}.</span></p>")
    execute(room, monster, Map.put(room_exit, "kind", "Normal"))
  end
  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"] do
    if Doors.open?(room, room_exit) do
      execute(room, monster, Map.put(room_exit, "kind", "Normal"))
    else
      Monster.send_scroll(monster, "<p><span class='red'>The #{String.downcase(kind)} is closed!</span></p>")
      room
    end
  end

  def execute(%Room{} = room, monster, %{"kind" => "Hidden", "passable_while_hidden" => true} = room_exit) do
    execute(room, monster, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => "Hidden", "passable_while_hidden" => false} = room_exit) do
    if Doors.open?(room, room_exit) do
      execute(room, monster, Map.put(room_exit, "kind", "Normal"))
    else
      Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
      room
    end
  end

  def execute(%Room{} = room, %Monster{} = monster, nil) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => "Command"}) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => "RemoteAction"}) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => "Normal", "destination" => destination_id}) do
    if !Monster.held(monster) and !Monster.confused(room, monster) do
      Room.display_exit_message(room, %{monster: monster, message: monster.exit_message, to: destination_id})

      destination_id
      |> RoomServer.find
      |> RoomServer.monster_entered(monster)

      put_in(room.monsters, Map.delete(room.monsters, monster.ref))
    else
      room
    end
  end

  def execute(%Room{} = room, %Monster{} = monster, %{"kind" => "Action", "destination" => destination_id} = room_exit) do
    if !Monster.held(monster) and !Monster.confused(room, monster) do

      Monster.send_scroll(monster, "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>")

      destination_id
      |> RoomServer.find
      |> RoomServer.monster_entered(monster, "<span class='yellow'>#{room_exit["to_message"]}</span>")

      Room.display_exit_message(room, %{monster: monster, message: room_exit["from_message"], to: destination_id})

      put_in(room.monsters, Map.delete(room.monsters, monster.ref))
    else
      room
    end
  end

end
