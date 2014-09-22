defmodule Commands.Look do
  use Systems.Command

  def keywords, do: ["look", "l"]

  def execute(spirit, arguments) do
    monster = Possession.possessed(spirit)

    entity = monster || spirit

    current_room = Parent.of(entity)

    if Enum.any? arguments do
      if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
        Systems.Description.add_description_to_scroll(entity, target)
      else
        send_message(entity, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(entity, current_room)
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.first(:name_contains, string)
  end

end
