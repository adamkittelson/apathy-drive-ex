defmodule ApathyDrive.Commands.Pick do
  use ApathyDrive.Command
  alias ApathyDrive.Doors

  def keywords, do: ["pick"]

  def execute(%Room{} = room, %{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Pick what?</p>")
    room
  end

  def execute(%Room{} = room, %{} = mobile, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction()

    room
    |> Room.get_exit(direction)
    |> pick(mobile, room)
  end

  defp pick(nil, %{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    room
  end

  defp pick(%{"kind" => kind} = room_exit, %{} = mobile, %Room{} = room)
       when kind in ["Door", "Gate", "Key"] do
    name = String.downcase(kind)

    cond do
      Doors.open?(room, room_exit) ->
        Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
        room

      pick?(
        room_exit,
        Mobile.attribute_at_level(mobile, :agility, mobile.level),
        lockpicking_modifier(mobile)
      ) ->
        mirror_pick!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>You successfully unlocked the #{name}.</p>")
        Mobile.send_scroll(mobile, "<p>You opened the #{name}.</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} pick open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        Room.open!(room, room_exit["direction"])

      true ->
        mirror_pick_fail!(room_exit, room.id)
        Mobile.send_scroll(mobile, "<p>Your attempts to pick the lock fail!</p>")

        Room.send_scroll(
          room,
          "<p>You see #{Mobile.colored_name(mobile)} attempt to pick open the #{name} #{
            ApathyDrive.Exit.direction_description(room_exit["direction"])
          }.</p>",
          [mobile]
        )

        room
    end
  end

  defp pick(_room_exit, %{} = mobile, %Room{} = room) do
    Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    room
  end

  defp lockpicking_modifier(mobile) do
    skill =
      case mobile.skills["lockpicking"] do
        %{level: level} ->
          level

        _ ->
          0
      end

    skill / mobile.level
  end

  defp pick?(%{"kind" => "Key"}, _agility, _modifier), do: false

  defp pick?(%{"difficulty" => difficulty}, agility, lockpicking) do
    agility * lockpicking + difficulty >= :rand.uniform(100)
  end

  defp mirror_pick!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_open(room_id, room_exit)
  end

  defp mirror_pick_fail!(%{"destination" => destination} = room_exit, room_id) do
    destination
    |> RoomServer.find()
    |> RoomServer.mirror_open_fail(room_id, room_exit)
  end
end
