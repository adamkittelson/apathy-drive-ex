defmodule ApathyDrive.Commands.Open do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["open"]

  def execute(%Room{} = room, %{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Open what?</p>")
    room
  end

  def execute(%Room{} = room, %{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction()

    room
    |> Room.get_exit(direction)
    |> open(mobile, room)
  end

  def mirror_open!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_open(room_id, room_exit)
  end

  defp open(nil, %{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp open(%{"kind" => kind} = room_exit, %{} = mobile, %Room{} = room)
       when kind in ["Door", "Gate", "Key"] do
    name = if kind == "Gate", do: "gate", else: "door"

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room

      !Room.unlocked?(room, room_exit["direction"]) ->
        open_fail!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>The #{name} is locked.</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} attempt to open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        room

      true ->
        mirror_open!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>You opened the #{name}.</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        Room.open!(room, room_exit["direction"])
    end
  end

  defp open(_room_exit, %{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp open_fail!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_open_fail(room_id, room_exit)
  end
end
