defmodule ApathyDrive.Commands.Lock do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["lock"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _message) do
    Mobile.body_required(mobile)

    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Lock what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> lock(mobile, room)
  end

  defp lock(nil, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp lock(%{"kind" => kind} = room_exit, %Mobile{} = mobile, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>You must close the #{name} before you may lock it.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already locked.</p>")
        room
      true ->
        mirror_lock!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>The #{name} is now locked.</p>")
        Room.send_scroll(room, "<p>You see #{Mobile.look_name(mobile)} lock the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", mobile)
        Room.lock!(room, room_exit["direction"])
    end
  end

  defp lock(_room_exit, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp mirror_lock!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_lock(room_id, room_exit)
  end

end
