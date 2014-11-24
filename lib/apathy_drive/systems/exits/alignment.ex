defmodule Systems.Exits.Alignment do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit), do: super(nil, monster, current_room, room_exit)

  def move(spirit, monster, current_room, room_exit) do
    min = room_exit["min"]
    max = room_exit["max"]
    case Components.Alignment.value(monster) do
      alignment when alignment < min ->
        send_message(monster, "scroll", "<p>You are not evil enough to use this exit.</p>")
      alignment when alignment > max ->
        send_message(monster, "scroll", "<p>You are too evil to use this exit.</p>")
      _ ->
        super(spirit, monster, current_room, room_exit)
    end
  end
end
