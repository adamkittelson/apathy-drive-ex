defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterItem, Item, Match, Mobile, Repo}

  def keywords, do: ["get"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Get what?</p>")
    room
  end

  def execute(%Room{rooms_items: items} = room, %Character{} = character, ["all"]) do
    items
    |> Enum.map(&(&1.item.name))
    |> get_all(room, character)
  end

  def execute(%Room{rooms_items: items, item_descriptions: item_descriptions} = room, %Character{} = character, args) do
    item = Enum.join(args, " ")

    actual_item = items
                  |> Enum.map(&(%{name: &1.item.name, room_item: &1}))
                  |> Match.one(:name_contains, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Match.one(:keyword_starts_with, item)

    case actual_item || visible_item || hidden_item do
      %{room_item: %{level: level, item: %Item{} = item} = room_item} ->

        character_item =
          %CharacterItem{
            character_id: character.id,
            item_id: item.id,
            level: level,
            strength: room_item.strength,
            agility: room_item.agility,
            intellect: room_item.intellect,
            willpower: room_item.willpower,
            health: room_item.health,
            charm: room_item.charm
          }

        Ecto.Multi.new
        |> Ecto.Multi.insert(:characters_items, character_item)
        |> Ecto.Multi.delete(:rooms_items, room_item)
        |> Repo.transaction

        room
        |> Repo.preload([rooms_items: :item], [force: true])
        |> Room.update_mobile(character.ref, fn(char) ->
             char
             |> Repo.preload([characters_items: :item], [force: true])
             |> Mobile.send_scroll("<p>You get #{Item.colored_name(item)}.</p>")
           end)
      %{name: psuedo_item} ->
        Mobile.send_scroll(character, "<p>#{psuedo_item |> capitalize_first} cannot be picked up.</p>")
        room
      nil ->
        Mobile.send_scroll(character, "<p>You don't see \"#{item}\" here.</p>")
        room
    end
  end

  defp get_all([], room, character) do
    Mobile.send_scroll(character, "<p>There is nothing here to get.</p>")
    room
  end

  defp get_all(item_names, room, character) do
    item_names
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         character = updated_room.mobiles[character.ref]
         execute(updated_room, character, [item_name])
       end)
  end
end
