defmodule Commands.Look do
  require Logger
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def keywords, do: ["look", "l"]

  def execute(%Spirit{} = spirit, arguments) do
    current_room = Spirit.find_room(spirit)
    if Enum.any? arguments do
      cond do
        Enum.member?(@directions, Enum.join(arguments, " ")) ->
          ApathyDrive.Exit.look(spirit, Enum.join(arguments, " "))
        target = current_room |> find_monster_in_room(Enum.join(arguments, " ")) ->
          Systems.Description.add_description_to_scroll(spirit, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          Spirit.send_scroll spirit, "<p>#{target}</p>"
        target = find_item_in_room(current_room, Enum.join(arguments, " ")) ->
          Spirit.send_scroll spirit, "<p>#{target.description}</p>"
      true ->
        Spirit.send_scroll(spirit, "<p>You do not notice that here.</p>")
      end
    else
      Room.look(current_room, spirit)
    end
    spirit
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)
    if Enum.any? arguments do
      cond do
        Enum.member?(@directions, Enum.join(arguments, " ")) ->
          ApathyDrive.Exit.look(monster, Enum.join(arguments, " "))
        target = current_room |> find_monster_in_room(Enum.join(arguments, " "), monster) ->
          Systems.Description.add_description_to_scroll(monster, target)
        target = current_room |> find_hidden_item_in_room(Enum.join(arguments, " ")) ->
          Monster.send_scroll monster, "<p>#{target}</p>"
        target = find_item_in_room(current_room, Enum.join(arguments, " ")) ->
          Monster.send_scroll monster, "<p>#{target.description}</p>"
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          Monster.send_scroll monster, "<p>#{target.description}</p>"
        true ->
          Monster.send_scroll(monster, "<p>You do not notice that here.</p>")
      end
    else
      Room.look(current_room, monster)
    end
    monster
  end

  defp find_monster_in_room(%Room{} = room, string) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_monster_in_room(%Room{} = room, string, %Monster{pid: pid} = monster) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Enum.map(fn(monster_pid) ->
         if monster_pid == pid do
           monster
         else
           monster_pid
         end
       end)
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_item_in_room(%Room{} = room, string) do
    room
    |> Room.items
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_item_on_monster(%Monster{} = monster, string) do
    (Monster.equipped_items(monster) ++ Monster.inventory(monster))
    |> Systems.Match.one(:name_contains, string)
  end

  defp find_hidden_item_in_room(%Room{item_descriptions: item_descriptions}, string) do
    key = item_descriptions
          |> Map.keys
          |> Enum.find(&(&1 == string))

    item_descriptions[key]
  end

end
