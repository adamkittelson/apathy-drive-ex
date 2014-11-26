defmodule Systems.Exits.Class do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit), do: super(nil, monster, current_room, room_exit)

  def move(spirit, monster, current_room, room_exit) do
    failure_message = room_exit["failure_message"] || "A strange power holds you back."
    room_message = room_exit["room_message"] || "{{User}} is stopped by an invisible force!"

    class = String.downcase(room_exit["class"])
    name = Components.Name.value(monster) |> String.downcase

    if String.contains?(name, class) do
      super(spirit, monster, current_room, room_exit)
    else
      send_message(monster, "scroll", "<p>#{failure_message}</p>")
      Systems.Monster.observers(current_room, monster)
      |> Enum.each(fn(observer) ->
        send_message(observer, "scroll", "<p><span class='yellow'>#{interpolate(room_message, %{"user" => monster})}</span></p>")
      end)
    end
  end
end
