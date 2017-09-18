defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterItem, Item, Match, Mobile, Repo}

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Equip what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["all"]) do
    character.inventory
    |> Enum.map(&(&1.name))
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         character = updated_room.mobiles[ref]
         execute(updated_room, character, [item_name])
       end)
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    item_name = Enum.join(args, " ")

    character.inventory
    |> Match.one(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" left unequipped.</p>")
           room
         %Item{} = item ->
           character =
             case equip_item(character, item) do
               %{equipped: equipped, unequipped: unequipped, character: character} ->
                 Enum.each(unequipped, fn(item) ->
                   Mobile.send_scroll(character, "<p>You remove #{Item.colored_name(item)}.</p>")
                 end)
                 Mobile.send_scroll(character, "<p>You are now wearing #{Item.colored_name(equipped)}.</p>")
                 send(character.socket, {:update_character, %{room_id: room.id, power: Mobile.power_at_level(character, character.level), level: character.level}})
                 character
               %{equipped: equipped, character: character} ->
                 Mobile.send_scroll(character, "<p>You are now wearing #{Item.colored_name(equipped)}.</p>")
                 send(character.socket, {:update_character, %{room_id: room.id, power: Mobile.power_at_level(character, character.level), level: character.level}})
                 character
               false ->
                 Mobile.send_scroll(character, "<p>You can't equip #{Item.colored_name(item)}.</p>")
             end

           put_in(room.mobiles[character.ref], character)
       end
  end

  def equip_item(%Character{inventory: inventory, equipment: equipment} = character, %Item{worn_on: worn_on} = item, persist \\ true) do

    cond do
      Enum.count(equipment, &(&1.worn_on == worn_on)) >= worn_on_max(item) ->
        item_to_remove =
          equipment
          |> Enum.sort_by(&Item.power_at_level(&1, &1.level))
          |> Enum.find(&(&1.worn_on == worn_on))

        equipment = List.delete(equipment, item_to_remove)

        inventory = List.delete(inventory, item)

        if persist do
          %CharacterItem{id: item_to_remove.characters_items_id}
          |> Ecto.Changeset.change(%{equipped: false})
          |> Repo.update!
        end

        inventory = List.insert_at(inventory, -1, item_to_remove)

        if persist do
          %CharacterItem{id: item.characters_items_id}
          |> Ecto.Changeset.change(%{equipped: true})
          |> Repo.update!
        end

        equipment = List.insert_at(equipment, -1, item)

        character =
          character
          |> Map.put(:inventory, inventory)
          |> Map.put(:equipment, equipment)

        %{equipped: item, unequipped: [item_to_remove], character: character}
      conflicting_worn_on(worn_on) |> Enum.any? ->
        items_to_remove =
          equipment
          |> Enum.filter(&(&1.worn_on in conflicting_worn_on(worn_on)))

        equipment = Enum.reject(equipment, &(&1 in items_to_remove))

        inventory = List.delete(inventory, item)

        if persist do
          Enum.each(items_to_remove, fn item_to_remove ->
            %CharacterItem{id: item_to_remove.characters_items_id}
            |> Ecto.Changeset.change(%{equipped: false})
            |> Repo.update!
          end)
        end

        inventory =
          items_to_remove
          |> Enum.reduce(inventory, fn(item_to_remove, inv) ->
               List.insert_at(inv, -1, item_to_remove)
             end)

        if persist do
          %CharacterItem{id: item.characters_items_id}
          |> Ecto.Changeset.change(%{equipped: true})
          |> Repo.update!
        end

        equipment = List.insert_at(equipment, -1, item)

        character =
          character
          |> Map.put(:inventory, inventory)
          |> Map.put(:equipment, equipment)

        %{equipped: item, unequipped: items_to_remove, character: character}
      true ->
        inventory =
          inventory
          |> List.delete(item)

        if persist do
          %CharacterItem{id: item.characters_items_id}
          |> Ecto.Changeset.change(%{equipped: true})
          |> Repo.update!
        end

        equipment =
          equipment
          |> List.insert_at(-1, item)

        character =
          character
          |> Map.put(:inventory, inventory)
          |> Map.put(:equipment, equipment)

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
