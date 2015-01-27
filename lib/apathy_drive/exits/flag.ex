defmodule ApathyDrive.Exits.Flag do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)
  def move(current_room, %Monster{} = monster, room_exit) do
    failure_message = room_exit["failure_message"] || "A strange power holds you back."
    min = room_exit["min"]
    max = room_exit["max"]
    case Components.Flags.value(monster)[room_exit["name"]] do
      nil ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      value when value >= min and value <= max ->
        send_message(monster, "scroll", "<p>#{failure_message}</p>")
      _ ->
        super(current_room, monster, room_exit)
    end
  end
end
