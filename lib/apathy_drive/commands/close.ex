defmodule ApathyDrive.Commands.Close do
  use ApathyDrive.Command
  alias ApathyDrive.{Doors, PubSub}

  def keywords, do: ["close", "shut"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Close what?</p>")
  end

  def execute(%Mobile{room_id: room_id} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room_id
    |> RoomServer.find
    |> RoomServer.close(%{pid: self(), name: Mobile.look_name(mobile)}, direction)
  end

  def execute(mobile, arguments) do
    Mobile.close(mobile, arguments)
  end

  def execute(%Room{} = room, mobile_data, direction) do
    room
    |> Room.get_exit(direction)
    |> close(mobile_data, room)
  end

  defp close(nil, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp close(%{"kind" => kind} = room_exit, %{pid: mobile, name: mobile_name}, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    if Doors.open?(room, room_exit) do
      data = %{
        closer: mobile,
        name: mobile_name,
        type: name,
        description: ApathyDrive.Exit.direction_description(room_exit["direction"]),
        direction: room_exit["direction"]
      }

      mirror_close!(room_exit, room.id)
      close!(room, data)
    else
      Mobile.send_scroll(mobile, "<p>The #{name} is already closed.</p>")
      room
    end
  end

  defp close(_room_exit, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp close!(%Room{id: id} = room, data) do
    PubSub.broadcast! "rooms:#{id}:mobiles", {:door_closed, data}
    Room.close!(room, data.direction)
  end

  defp mirror_close!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_close(room_id, room_exit)
  end
end
