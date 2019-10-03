defmodule ApathyDrive.Commands.Search do
  use ApathyDrive.Command

  def keywords, do: ["sea", "search"]

  def execute(%Room{} = room, %Character{} = character, []) do
    detected_characters =
      room.mobiles
      |> Map.values()
      |> Stream.filter(&(&1.__struct__ == Character))
      |> Stream.reject(&(&1.ref == character.ref))
      |> Stream.filter(& &1.sneaking)
      |> Stream.reject(&(&1.ref in character.detected_characters))
      |> Enum.reduce([], fn sneaker, list ->
        if Mobile.detected?(character, sneaker, room) do
          Mobile.send_scroll(
            character,
            "<p>Your search reveals #{Mobile.colored_name(sneaker)} hiding in the shadows.</p>"
          )

          [sneaker.ref | list]
        else
          list
        end
      end)

    if Enum.any?(detected_characters) do
      room =
        Enum.reduce(detected_characters, room, fn detected, room ->
          Room.update_mobile(room, character.ref, fn _room, character ->
            update_in(character.detected_characters, &MapSet.put(&1, detected))
          end)
        end)

      Room.update_moblist(room)
      room
    else
      Mobile.send_scroll(character, "<p>Your search revealed nothing.</p>")
      room
    end
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    direction =
      arguments
      |> Enum.join(" ")
      |> Room.direction()

    room_exit = Room.get_exit(room, direction)

    case room_exit do
      %{"kind" => "Hidden"} ->
        search(room, character, room_exit)

      _ ->
        Mobile.send_scroll(
          character,
          "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>"
        )

        room
    end
  end

  defp search(
         %Room{} = room,
         %Character{} = character,
         %{"direction" => direction, "searchable" => searchable?} = room_exit
       ) do
    if Room.searched?(room, room_exit) || !searchable? || :rand.uniform(100) < 75 do
      Mobile.send_scroll(
        character,
        "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>"
      )

      room
    else
      if message = room_exit["message_when_revealed"] do
        Mobile.send_scroll(character, "<p>#{message}</p>")
      else
        Mobile.send_scroll(
          character,
          "<p>You found an exit #{ApathyDrive.Exit.direction_description(direction)}!</p>"
        )
      end

      Systems.Effect.add(room, %{searched: direction}, 300_000)
    end
  end
end
