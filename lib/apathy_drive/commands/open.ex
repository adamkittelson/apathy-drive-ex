defmodule Commands.Open do
  use ApathyDrive.Command

  def keywords, do: ["open"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Open what?</p>")
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
        Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
      %{"kind" => "Door"} ->
        ApathyDrive.Exits.Door.open(mobile, room, room_exit)
      %{"kind" => "Gate"} ->
        ApathyDrive.Exits.Gate.open(mobile, room, room_exit)
      %{"kind" => "Key"} ->
        ApathyDrive.Exits.Key.open(mobile, room, room_exit)
      _ ->
        Mobile.send_scroll(mobile, "<p>That exit has no door.</p>")
    end
  end

end
