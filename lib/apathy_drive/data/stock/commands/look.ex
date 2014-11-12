defmodule Commands.Look do
  use Systems.Command

  def keywords, do: ["look", "l"]

  def execute(spirit, nil, arguments) do
    current_room = Parent.of(spirit)

    if Enum.any? arguments do
      cond do
        target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) ->
          Systems.Description.add_description_to_scroll(spirit, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
      true ->
        send_message(spirit, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(spirit, current_room)
    end
  end

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      cond do
        target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) ->
          Systems.Description.add_description_to_scroll(monster, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          send_message monster, "scroll", "<p>#{Components.Description.value(target)}</p>"
      true ->
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

  defp find_hidden_item_in_room(room, string) do
    room
    |> Components.ItemDescriptions.get_item_descriptions
    |> Systems.Match.one(:name_contains, string)
  end

end
