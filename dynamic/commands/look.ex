defmodule Commands.Look do
  use Systems.Command

  def keywords, do: ["look", "l"]

  def execute(entity, arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)

    if Enum.any? arguments do
      if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
        Systems.Description.add_description_to_scroll(entity, target)
      else
        Components.Player.send_message(entity, ["scroll", "<p>You do not notice that here.</p>"])
      end
    else
      Systems.Room.display_room_in_scroll(entity, current_room)
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.entities_in_room
    |> Systems.Match.first(:name_contains, string)
  end

end
