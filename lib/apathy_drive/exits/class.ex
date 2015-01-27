defmodule ApathyDrive.Exits.Class do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)
  def move(current_room, %Monster{} = monster, room_exit) do
    class = String.downcase(room_exit["class"])
    name = Components.Name.value(monster) |> String.downcase

    if String.contains?(name, class) do
      super(current_room, monster, room_exit)
    else
      send_message(monster, "scroll", "<p>Only #{Inflex.pluralize(room_exit["class"])} may go #{room_exit["direction"]} here.</p>")
      Systems.Monster.observers(current_room, monster)
      |> Enum.each(fn(observer) ->
        send_message(observer, "scroll", "<p>#{capitalize_first(Components.Name.value(monster))} tries to go #{room_exit["direction"]} but only #{Inflex.pluralize(room_exit["class"])} may go that way.</span></p>")
      end)
    end
  end
end
