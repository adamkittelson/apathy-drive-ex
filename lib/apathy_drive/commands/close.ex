defmodule ApathyDrive.Commands.Close do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["close"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _message) do
    Monster.body_required(monster)

    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Close what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> close(monster, room)
  end

  defp close(nil, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction!</p>")
    room
  end

  defp close(%{"kind" => kind} = room_exit, %Monster{} = monster, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        mirror_close!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>You closed the #{name}.</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} close the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", [monster])
        Room.close!(room, room_exit["direction"])
      true ->
        Monster.send_scroll(monster, "<p>The #{name} is already closed.</p>")
        room
    end
  end

  defp close(_room_exit, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>That exit has no door.</p>")
    room
  end

  defp mirror_close!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_close(room_id, room_exit)
  end

end
