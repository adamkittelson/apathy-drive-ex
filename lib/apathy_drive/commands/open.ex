defmodule ApathyDrive.Commands.Open do
  use ApathyDrive.Command
  alias ApathyDrive.{Doors, PubSub}

  def keywords, do: ["open"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Open what?</p>")
  end

  def execute(%Mobile{room_id: room_id} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room_id
    |> Room.find
    |> Room.open(%{pid: self(), name: Mobile.look_name(mobile)}, direction)
  end

  def execute(mobile, arguments) do
    Mobile.open(mobile, arguments)
  end

  def execute(%Room{} = room, mobile_data, direction) do
    room
    |> Room.get_exit(direction)
    |> open(mobile_data, room)
  end

  defp open(nil, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp open(%{"kind" => kind} = room_exit, %{pid: mobile, name: mobile_name}, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is locked.</p>")
        room
      true ->
        data = %{
          opener: mobile,
          name: mobile_name,
          type: name,
          description: ApathyDrive.Exit.direction_description(room_exit["direction"]),
          direction: room_exit["direction"]
        }

        mirror_open!(room_exit, room.id)
        open!(room, data)
    end
  end

  defp open(_room_exit, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp open!(%Room{id: id} = room, data) do
    PubSub.broadcast! "rooms:#{id}:mobiles", {:door_opened, data}
    Room.open!(room, data.direction)
  end

  defp mirror_open!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> Room.find
    |> Room.mirror_open(room_id, room_exit)
  end

end
