defmodule ApathyDrive.Commands.Bash do
  use ApathyDrive.Command
  alias ApathyDrive.{Doors, PubSub}

  def keywords, do: ["bash"]

  def execute(%Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Bash what?</p>")
  end

  def execute(%Mobile{room_id: room_id} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room_id
    |> Room.find
    |> Room.bash(%{pid: self(), strength: Mobile.strength(mobile), name: Mobile.look_name(mobile)}, direction)
  end

  def execute(mobile, arguments) do
    Mobile.bash(mobile, arguments)
  end

  def execute(%Room{} = room, mobile_data, direction) do
    room
    |> Room.get_exit(direction)
    |> bash(mobile_data, room)
  end

  defp bash(nil, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp bash(%{"kind" => kind} = room_exit, %{pid: mobile, strength: strength, name: mobile_name}, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room
      bash?(room_exit, strength) ->
        data = %{
          basher: mobile,
          name: mobile_name,
          type: name,
          description: ApathyDrive.Exit.direction_description(room_exit["direction"]),
          direction: room_exit["direction"]
        }

        mirror_bash!(room_exit, room.id)
        bash!(room, data)
      true ->
        Mobile.send_scroll(mobile, "<p>Your attempts to bash through fail!</p>")
        PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
        |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} attempt to bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

        mirror_room = Room.find(room_exit["destination"])
        mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

        if mirror_exit["kind"] == room_exit["kind"] do
          PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
          |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} shudders from an impact, but it holds!</p>")))

        end

        # if :random.uniform(3) == 3 do
        #   Monster.send_scroll(monster, "<p>You take #{amount} damage for bashing the #{name}!</p>")
        # end
    end
  end

  defp bash(_room_exit, %{pid: mobile}, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp bash?(%{"difficulty" => difficulty}, strength) do
    (strength + difficulty) >= :rand.uniform(100)
  end

  defp bash!(%Room{id: id} = room, data) do
    PubSub.broadcast! "rooms:#{id}:mobiles", {:door_bashed, data}
    Room.open!(room, data.direction)
  end

  defp mirror_bash!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> Room.find
    |> Room.mirror_bash(room_id, room_exit)
  end

end
