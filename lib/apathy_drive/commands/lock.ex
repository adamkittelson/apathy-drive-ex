defmodule ApathyDrive.Commands.Lock do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["lock"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _message) do
    Monster.body_required(monster)

    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Lock what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> lock(monster, room)
  end

  defp lock(nil, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction!</p>")
    room
  end

  defp lock(%{"kind" => kind} = room_exit, %Monster{} = monster, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Monster.send_scroll(monster, "<p>You must close the #{name} before you may lock it.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        Monster.send_scroll(monster, "<p>The #{name} is already locked.</p>")
        room
      true ->
        mirror_lock!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>The #{name} is now locked.</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} lock the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", monster)
        Room.lock!(room, room_exit["direction"])
    end
  end

  defp lock(_room_exit, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>That exit has no door.</p>")
    room
  end

  defp mirror_lock!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_lock(room_id, room_exit)
  end

end
