defmodule ApathyDrive.Commands.Open do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["open"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _message) do
    Mobile.send_scroll(mobile, "<p>You need a body to open doors, however, given that you don't have a body, you can simply pass right through them.</p>")

    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Open what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> open(mobile, room)
  end

  defp open(nil, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp open(%{"kind" => kind} = room_exit, %Mobile{} = mobile, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        open_fail!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>The #{name} is locked.</p>")
        Room.send_scroll(room, "<p>You see #{Mobile.look_name(mobile)} attempt to open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", mobile)
        room
      true ->
        mirror_open!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>You bashed the #{name} open.</p>")
        Room.send_scroll(room, "<p>You see #{Mobile.look_name(mobile)} bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", mobile)
        Room.open!(room, room_exit["direction"])
    end
  end

  defp open(_room_exit, %Mobile{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp mirror_open!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_open(room_id, room_exit)
  end

  defp open_fail!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_open_fail(room_id, room_exit)
  end

end
