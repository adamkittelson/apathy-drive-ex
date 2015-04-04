defmodule Commands.Where do
  use ApathyDrive.Command
  alias Systems.Room

  def keywords, do: ["where"]

  def execute(spirit, _monster, arguments) do
    Task.start fn ->
      Spirit.send_scroll(spirit, "<p>searching...</p>")
      if Enum.any?(arguments) do
        query = Enum.join(arguments, " ")
        matches = Enum.filter(Room.all, fn(room) ->
                    room_matches?(room, query) or
                    monster_matches?(room, query) or
                    item_matches?(room, query)
                  end)
        case matches  do
          [] ->
            Spirit.send_scroll(spirit, "<p>no matches found</p>")
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

  def room_matches?(room, query) do
    Systems.Match.name_contains(query, room)
  end

  def monster_matches?(room, query) do
    monster = room
              |> Systems.Room.living_in_room
              |> Systems.Match.one(:name_contains, query)
    !!monster
  end

  def item_matches?(room, query) do
    !!item_in_room(room, query) or !!item_on_monster(room, query)
  end

  def item_in_room(room, query) do
    room
    |> Components.Items.get_items
    |> Systems.Match.one(:name_contains, query)
  end

  def item_on_monster(room, query) do
    room
    |> Systems.Room.living_in_room
    |> Enum.find(fn(monster) ->
         (Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster))
         |> Systems.Match.one(:name_contains, query)
       end)
  end

  def display_room(room, spirit) do
    directions = room
                 |> Room.exit_directions
                 |> Room.exit_directions_html


    Spirit.send_scroll(spirit, "<br>")
    Spirit.send_scroll(spirit, "<p><span class='white'>Room ##{Components.ID.value(room)}</span></p>")
    Spirit.send_scroll(spirit, "<div class='room'><div class='title'>#{Components.Name.value(room)}</div>#{Room.items_html(room)}#{Room.entities_html(spirit, room)}#{directions}</div>")
  end

end
