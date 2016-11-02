defmodule ApathyDrive.Commands.Open do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["open"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _message) do
    Monster.send_scroll(monster, "<p>You need a body to open doors, however, given that you don't have a body, you can simply pass right through them.</p>")

    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Open what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> open(monster, room)
  end

  defp open(nil, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction!</p>")
    room
  end

  defp open(%{"kind" => kind} = room_exit, %Monster{} = monster, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Monster.send_scroll(monster, "<p>The #{name} is already open.</p>")
        room
      !Room.unlocked?(room, room_exit["direction"]) ->
        open_fail!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>The #{name} is locked.</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} attempt to open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", [monster])
        room
      true ->
        mirror_open!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>You bashed the #{name} open.</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", [monster])
        Room.open!(room, room_exit["direction"])
    end
  end

  defp open(_room_exit, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>That exit has no door.</p>")
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
