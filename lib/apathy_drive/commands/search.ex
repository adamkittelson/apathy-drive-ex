defmodule ApathyDrive.Commands.Search do
  use ApathyDrive.Command

  def keywords, do: ["sea", "search"]

  def execute(%Room{}, mobile, []) do
    Mobile.send_scroll(mobile, "<p>Your search revealed nothing.</p>")
  end

  def execute(%Room{} = room, mobile, arguments) do
    direction = arguments
                |> Enum.join(" ")
                |> Room.direction

    room_exit = Room.get_exit(room, direction)

    case room_exit do
      %{"kind" => "Hidden"} ->
        search(room, mobile, room_exit)
      _ ->
        Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    end
  end

  defp search(%Room{} = room, mobile, %{"direction" => direction, "message_when_revealed" => message, "searchable" => searchable?} = room_exit) do
    if Room.searched?(room, room_exit) || !searchable? || :rand.uniform(100) < 75 do
      Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    else
      if message do
        Mobile.send_scroll(mobile, "<p>#{message}</p>")
      end

      Room.search(self, room_exit["direction"])
    end
  end

end
