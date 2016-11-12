defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, Match, Mobile, Repo}

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Equip what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["all"]) do
    character.inventory
    |> Enum.map(&(&1.item.name))
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         character = updated_room.mobiles[ref]
         execute(updated_room, character, [item_name])
       end)
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    item_name = Enum.join(args, " ")

    character.inventory
    |> Enum.map(&(%{name: &1.name, item: &1}))
    |> Match.one(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" left unequipped.</p>")
           room
         %{item: item} ->
           character =
             case equip_item(character, item) do
               %{equipped: equipped, unequipped: unequipped, character: character} ->
                 Enum.each(unequipped, fn(item) ->
                   Mobile.send_scroll(character, "<p>You remove #{item.item.name}.</p>")
                 end)
                 Mobile.send_scroll(character, "<p>You are now wearing #{equipped.item.name}.</p>")
                 character
               %{equipped: equipped, character: character} ->
                 Mobile.send_scroll(character, "<p>You are now wearing #{equipped.item.name}.</p>")
                 character
             end

           put_in(room.mobiles[character.ref], character)
       end
  end

  def equip_item(%Character{inventory: inventory, equipment: equipment} = character, %Item{worn_on: worn_on} = item) do

    cond do
      Enum.count(equipment, &(&1.worn_on == worn_on)) >= worn_on_max(item) ->
        item_to_remove =
          equipment
          |> Enum.find(&(&1.worn_on == worn_on))

        equipment = List.delete(equipment, item_to_remove)

        inventory = List.delete(inventory, item)

        item_to_remove =
          put_in(item_to_remove.equipped, false)
          |> Repo.save!

        inventory = List.insert_at(inventory, -1, item_to_remove)

        item =
          put_in(item.equipped, true)
          |> Repo.save!

        equipment = List.insert_at(equipment, -1, item)

        character = put_in(character.characters_items, equipment ++ inventory)

        %{equipped: item, unequipped: [item_to_remove], character: character}
      conflicting_worn_on(worn_on) |> Enum.any? ->
        items_to_remove =
          equipment
          |> Enum.filter(&(&1.item.worn_on in conflicting_worn_on(worn_on)))

        equipment = Enum.reject(equipment, &(&1 in items_to_remove))

        inventory = List.delete(inventory, item)

        items_to_remove =
          Enum.map(items_to_remove, fn item_to_remove ->
            put_in(item_to_remove.equipped, false)
            |> Repo.save!
          end)

        inventory =
          items_to_remove
          |> Enum.reduce(inventory, fn(item_to_remove, inv) ->
               List.insert_at(inv, -1, item_to_remove)
             end)

        item =
          put_in(item.equipped, true)
          |> Repo.save!

        equipment = List.insert_at(equipment, -1, item)

        character = put_in(character.characters_items, equipment ++ inventory)

        %{equipped: item, unequipped: items_to_remove, character: character}
      true ->
        inventory =
          inventory
          |> List.delete(item)

        item =
          put_in(item.equipped, true)
          |> Repo.save!

        equipment =
          equipment
          |> List.insert_at(-1, item)

        character = put_in(character.characters_items, equipment ++ inventory)

        %{equipped: item, character: character}
    end
  end

  defp worn_on_max(%{worn_on: "Finger"}), do: 2
  defp worn_on_max(%{worn_on: "Wrist"}),  do: 2
  defp worn_on_max(%{worn_on: _}),        do: 1

  defp conflicting_worn_on("Weapon Hand"), do: ["Two Handed"]
  defp conflicting_worn_on("Off-Hand"), do: ["Two Handed"]
  defp conflicting_worn_on("Two Handed"), do: ["Weapon Hand", "Off-Hand"]
  defp conflicting_worn_on(_), do: []

end
