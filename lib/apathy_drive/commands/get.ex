defmodule ApathyDrive.Commands.Get do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["get"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Get what?</p>")
    room
  end

  def execute(%Room{items: items} = room, %Character{} = character, ["all"]) do
    items
    |> Enum.map(& &1.name)
    |> get_all(room, character)
  end

  def execute(
        %Room{items: items, item_descriptions: item_descriptions} = room,
        %Character{} = character,
        args
      ) do
    item = Enum.join(args, " ")

    actual_item =
      items
      |> Match.one(:name_contains, item)

    visible_item =
      item_descriptions["visible"]
      |> Map.keys()
      |> Enum.map(&%{name: &1, keywords: String.split(&1)})
      |> Match.one(:keyword_starts_with, item)

    hidden_item =
      item_descriptions["hidden"]
      |> Map.keys()
      |> Enum.map(&%{name: &1, keywords: String.split(&1)})
      |> Match.one(:keyword_starts_with, item)

    case actual_item || visible_item || hidden_item do
      %Item{instance_id: instance_id} = item ->
        ItemInstance
        |> Repo.get(instance_id)
        |> Ecto.Changeset.change(%{
          room_id: nil,
          character_id: character.id,
          equipped: false,
          hidden: false
        })
        |> Repo.update!()

        update_in(room.items, &List.delete(&1, item))
        |> Room.update_mobile(character.ref, fn char ->
          char
          |> update_in([Access.key!(:inventory)], &[item | &1])
          |> Mobile.send_scroll("<p>You get #{Item.colored_name(item)}.</p>")
        end)

      %{name: psuedo_item} ->
        Mobile.send_scroll(
          character,
          "<p>#{psuedo_item |> capitalize_first} cannot be picked up.</p>"
        )

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
    |> Enum.reduce(room, fn item_name, updated_room ->
      character = updated_room.mobiles[character.ref]
      execute(updated_room, character, [item_name])
    end)
  end
end
