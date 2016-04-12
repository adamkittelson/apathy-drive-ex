defmodule ApathyDrive.Commands.Lock do
  use ApathyDrive.Command
  alias ApathyDrive.{Doors, PubSub}

  def keywords, do: ["lock"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Lock what?</p>")
  end

  def execute(%Mobile{room_id: room_id} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room_id
    |> Room.find
    |> Room.lock(%{pid: self(), name: Mobile.look_name(mobile)}, direction)
  end

  def execute(mobile, arguments) do
    Mobile.lock(mobile, arguments)
  end

  def execute(%Room{} = room, mobile_data, direction) do
    room
    |> Room.get_exit(direction)
    |> lock(mobile_data, room)
  end

  defp lock(nil, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp lock(%{"kind" => kind} = room_exit, %{pid: mobile, name: mobile_name}, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>You must close the #{name} before you may lock it.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already locked.</p>")
        room
      true ->
        data = %{
          locker: mobile,
          name: mobile_name,
          type: name,
          description: ApathyDrive.Exit.direction_description(room_exit["direction"]),
          direction: room_exit["direction"]
        }

        mirror_lock!(room_exit, room.id)
        lock!(room, data)
    end
  end

  defp lock(_room_exit, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp lock!(%Room{id: id} = room, data) do
    PubSub.broadcast! "rooms:#{id}:mobiles", {:door_locked, data}
    Room.lock!(room, data.direction)
  end

  defp mirror_lock!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> Room.find
    |> Room.mirror_lock(room_id, room_exit)
  end
end
