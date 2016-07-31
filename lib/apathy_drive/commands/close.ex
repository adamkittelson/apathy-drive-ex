defmodule ApathyDrive.Commands.Close do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["close"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _message) do
    Mobile.body_required(mobile)

    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Close what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> close(mobile, room)
  end

  defp close(nil, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp close(%{"kind" => kind} = room_exit, %Mobile{} = mobile, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        mirror_close!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>You closed the #{name}.</p>")
        Room.send_scroll(room, "<p>You see #{Mobile.look_name(mobile)} close the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", mobile)
        Room.close!(room, room_exit["direction"])
      true ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already closed.</p>")
        room
    end
  end

  defp close(_room_exit, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp mirror_close!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_close(room_id, room_exit)
  end

end
