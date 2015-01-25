defmodule ApathyDrive.Exits.Flag do
  use ApathyDrive.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit) do
    failure_message = room_exit["failure_message"] || "A strange power holds you back."
    min = room_exit["min"]
    max = room_exit["max"]
    case Components.Flags.value(monster)[room_exit["name"]] do
      nil ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      value when value >= min and value <= max ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      _ ->
        super(nil, monster, current_room, room_exit)
    end
  end

  def move(spirit, monster, current_room, room_exit) do
    failure_message = room_exit["failure_message"] || "A strange power holds you back."
    min = room_exit["min"]
    max = room_exit["max"]
    case Components.Flags.value(monster)[room_exit["name"]] do
      nil ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      value when value >= min and value <= max ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      _ ->
        super(spirit, monster, current_room, room_exit)
    end
  end
end
