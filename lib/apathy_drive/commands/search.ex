defmodule ApathyDrive.Commands.Search do
  use ApathyDrive.Command

  def keywords, do: ["sea", "search"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Your search revealed nothing.</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    direction = arguments
                |> Enum.join(" ")
                |> Room.direction

    room_exit = Room.get_exit(room, direction)

    case room_exit do
      %{"kind" => "Hidden"} ->
        search(room, mobile, room_exit)
      _ ->
        Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
        room
    end
  end

  defp search(%Room{} = room, %Mobile{} = mobile, %{"direction" => direction, "searchable" => searchable?} = room_exit) do
    if Room.searched?(room, room_exit) || !searchable? || :rand.uniform(100) < 75 do
      Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
      room
    else
      if message = room_exit["message_when_revealed"] do
        Mobile.send_scroll(mobile, "<p>#{message}</p>")
      else
        Mobile.send_scroll(mobile, "<p>You found an exit #{ApathyDrive.Exit.direction_description(direction)}!</p>")
      end

      Systems.Effect.add(room, %{searched: direction}, 300)
    end
  end

end
