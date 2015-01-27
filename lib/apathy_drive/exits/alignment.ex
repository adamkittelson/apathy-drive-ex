defmodule ApathyDrive.Exits.Alignment do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)
  def move(current_room, %Monster{} = monster, room_exit) do
    min = room_exit["min"]
    max = room_exit["max"]
    case Components.Alignment.value(monster) do
      alignment when alignment < min ->
        send_message(monster, "scroll", "<p>You are not evil enough to use this exit.</p>")
      alignment when alignment > max ->
        send_message(monster, "scroll", "<p>You are too evil to use this exit.</p>")
      _ ->
        super(current_room, monster, room_exit)
    end
  end
end
