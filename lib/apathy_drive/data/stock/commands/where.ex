defmodule Commands.Where do
  use Systems.Command
  alias Systems.Room

  def keywords, do: ["where"]

  def execute(spirit, monster, arguments) do
    Task.start fn ->
      send_message(spirit, "scroll", "<p>searching...</p>")
      if Enum.any?(arguments) do
        case Systems.Match.all(Rooms.all, :name_contains, Enum.join(arguments, " ")) do
          [] ->
            send_message(spirit, "scroll", "<p>no matches found</p>")
          matches ->
            Enum.each(matches, &(display_room(&1, spirit)))
        end
      else
        spirit
        |> Parent.of
        |> display_room(spirit)
      end
    end
  end

  def display_room(room, spirit) do
    directions = room
                 |> Room.exit_directions
                 |> Room.exit_directions_html


    send_message(spirit, "scroll", "<br>")
    send_message(spirit, "scroll", "<p><span class='white'>Room ##{Components.ID.value(room)}</span></p>")
    send_message(spirit, "scroll", "<div class='room'><div class='title'>#{Components.Name.value(room)}</div>#{Room.items_html(room)}#{Room.entities_html(spirit, room)}#{directions}</div>")
    
  end

end
