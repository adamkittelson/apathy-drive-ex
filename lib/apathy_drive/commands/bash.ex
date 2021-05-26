defmodule ApathyDrive.Commands.Bash do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Doors}

  def keywords, do: ["bash"]

  def execute(%Room{} = room, %{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Bash what?</p>")
    room
  end

  def execute(%Room{} = room, %{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction()

    room
    |> Room.get_exit(direction)
    |> bash(mobile, room, arguments)
  end

  defp bash(nil, %{} = mobile, %Room{} = room, arguments) do
    if skill = mobile.skills["bash"] do
      ability = skill.module.ability(mobile)
      Ability.execute(room, mobile.ref, ability, Enum.join(arguments, " "))
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
      room
    end
  end

  defp bash(%{"kind" => kind} = room_exit, %{} = mobile, %Room{} = room, _args)
       when kind in ["Door", "Gate", "Key"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room

      bash?(room_exit, Mobile.attribute_at_level(mobile, :strength, mobile.level)) ->
        mirror_bash!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>You bashed the #{name} open.</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} bash open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        Room.open!(room, room_exit["direction"])

      true ->
        mirror_bash_fail!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>Your attempts to bash through fail!</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} attempt to bash open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        room
    end
  end

  defp bash(_room_exit, %{} = mobile, %Room{} = room, _args) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp bash?(%{"kind" => "Key"}, _strength), do: false

  defp bash?(%{"difficulty" => difficulty}, strength) do
    strength + difficulty >= :rand.uniform(100)
  end

  defp mirror_bash!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_bash(room_id, room_exit)
  end

  defp mirror_bash_fail!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_bash_fail(room_id, room_exit)
  end
end
