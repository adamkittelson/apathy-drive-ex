defmodule Commands.Look do
  use Systems.Command

  def keywords, do: ["look", "l"]

  def execute(spirit, nil, arguments) do
    current_room = Parent.of(spirit)

    if Enum.any? arguments do
      if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
        Systems.Description.add_description_to_scroll(spirit, target)
      else
        send_message(spirit, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(spirit, current_room)
    end
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
        Systems.Description.add_description_to_scroll(monster, target)
      else
        send_message(monster, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(monster, current_room)
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.one(:name_contains, string)
  end

end
