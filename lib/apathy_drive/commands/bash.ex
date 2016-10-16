defmodule ApathyDrive.Commands.Bash do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["bash"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _message) do
    Monster.send_scroll(monster, "<p>You need a body to bash doors open, however, given that you don't have a body, you can simply pass right through them.</p>")

    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Bash what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction

    room
    |> Room.get_exit(direction)
    |> bash(monster, room)
  end

  defp bash(nil, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction!</p>")
    room
  end

  defp bash(%{"kind" => kind} = room_exit, %Monster{} = monster, %Room{} = room) when kind in ["Door", "Gate"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Monster.send_scroll(monster, "<p>The #{name} is already open.</p>")
        room
      bash?(room_exit, Monster.strength(monster)) ->
        mirror_bash!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>You bashed the #{name} open.</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", monster)
        Room.open!(room, room_exit["direction"])
      true ->
        mirror_bash_fail!(room_exit, room.id)
        Monster.send_scroll(monster, "<p>Your attempts to bash through fail!</p>")
        Room.send_scroll(room, "<p>You see #{Monster.look_name(monster)} attempt to bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>", monster)
        room
    end
  end

  defp bash(_room_exit, %Monster{} = monster, %Room{} = room) do
    Monster.send_scroll(monster, "<p>That exit has no door.</p>")
    room
  end

  defp bash?(%{"difficulty" => difficulty}, strength) do
    (strength + difficulty) >= :rand.uniform(100)
  end

  defp mirror_bash!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_bash(room_id, room_exit)
  end

  defp mirror_bash_fail!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find
    |> RoomServer.mirror_bash_fail(room_id, room_exit)
  end

end
