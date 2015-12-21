defmodule Commands.Search do
  use ApathyDrive.Command

  def keywords, do: ["sea", "search"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Your search revealed nothing.</p>")
  end

  def execute(mobile, arguments) do
    direction = arguments
                |> Enum.join(" ")
                |> Room.direction

    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    room_exit = Room.get_exit(room, direction)

    case room_exit do
      nil ->
        Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
      %{"kind" => "Hidden"} ->
        ApathyDrive.Exits.Hidden.search(mobile, room, room_exit)
      _ ->
        Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    end
  end

end
