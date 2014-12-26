defmodule Commands.Look do
  use Systems.Command

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def keywords, do: ["look", "l"]

  def execute(spirit, nil, arguments) do
    current_room = Parent.of(spirit)

    if Enum.any? arguments do
      cond do
        Enum.member?(@directions, Enum.join(arguments, " ")) ->
          Systems.Exit.look(spirit, nil, Enum.join(arguments, " "))
        target = current_room |> find_monster_in_room(Enum.join(arguments, " ")) ->
          Systems.Description.add_description_to_scroll(spirit, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
        target = find_item_in_room(current_room, Enum.join(arguments, " ")) ->
          if Entity.has_component?(target, Components.Description) do
            send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
          else
            send_message spirit, "scroll", "<p>#{Components.Module.value(target).description}</p>"
          end
      true ->
        send_message(spirit, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(spirit, current_room)
    end
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      cond do
        Enum.member?(@directions, Enum.join(arguments, " ")) ->
          Systems.Exit.look(spirit, monster, Enum.join(arguments, " "))
        target = current_room |> find_monster_in_room(Enum.join(arguments, " ")) ->
          Systems.Description.add_description_to_scroll(monster, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
        target = find_item_in_room(current_room, Enum.join(arguments, " ")) ->
          if Entity.has_component?(target, Components.Description) do
            send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
          else
            send_message spirit, "scroll", "<p>#{Components.Module.value(target).description}</p>"
          end
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          if Entity.has_component?(target, Components.Description) do
            send_message spirit, "scroll", "<p>#{Components.Description.value(target)}</p>"
          else
            send_message spirit, "scroll", "<p>#{Components.Module.value(target).description}</p>"
          end
      true ->
        send_message(monster, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      Systems.Room.display_room_in_scroll(monster, current_room)
    end
  end

  defp find_monster_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_item_in_room(room, string) do
    room
    |> Components.Items.get_items
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_item_on_monster(monster, string) do
    (Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster))
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_hidden_item_in_room(room, string) do
    room
    |> Components.ItemDescriptions.get_item_descriptions
    |> Systems.Match.one(:name_contains, string)
  end

end
