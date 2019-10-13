defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["wear", "equip", "eq", "wield", "arm", "ar"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Equip what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["all"]) do
    character.inventory
    |> Enum.reject(&is_nil(&1.worn_on))
    |> Enum.map(& &1.name)
    |> Enum.reduce(room, fn item_name, updated_room ->
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

      %{type: "Light"} = item ->
        ApathyDrive.Commands.Use.execute(room, character, [item.name])

      %{} = item ->
        character =
          case equip_item(character, item) do
            %{equipped: equipped, unequipped: unequipped, character: character} ->
              Enum.each(unequipped, fn item ->
                Mobile.send_scroll(
                  character,
                  "<p>You remove #{Item.colored_name(item, character: character)}.</p>"
                )
              end)

              Mobile.send_scroll(
                character,
                "<p>You are now wearing #{Item.colored_name(equipped, character: character)}.</p>"
              )

              send(
                character.socket,
                {:update_character,
                 %{
                   room_id: room.id,
                   power: Mobile.power_at_level(character, character.level),
                   level: character.level
                 }}
              )

              character

            %{equipped: equipped, character: character} ->
              Mobile.send_scroll(
                character,
                "<p>You are now wearing #{Item.colored_name(equipped, character: character)}.</p>"
              )

              send(
                character.socket,
                {:update_character,
                 %{
                   room_id: room.id,
                   power: Mobile.power_at_level(character, character.level),
                   level: character.level
                 }}
              )

              character

            false ->
              Mobile.send_scroll(
                character,
                "<p>You cannot equip #{Item.colored_name(item, character: character)}.</p>"
              )
          end

        character = Character.assign_limbs_to_equipment(character)

        room = put_in(room.mobiles[character.ref], character)
        Character.update_score(character, room)
        Room.update_hp_bar(room, character.ref)
        Room.update_mana_bar(room, character.ref)
        room
    end
  end

  def limbs_for_slot(character, slot) do
    character.limbs
    |> Map.keys()
    |> Enum.filter(&(slot in character.limbs[&1].slots))
  end

  def worn_on_max(%{worn_on: slot})
      when slot in ["Finger", "Wrist", "Foot", "Arm", "Hand", "Held"],
      do: 2

  def worn_on_max(%{worn_on: _}), do: 1

  def worn_on_max(%Character{} = character, %Item{} = item) do
    missing_limbs =
      character
      |> limbs_for_slot(item.worn_on)
      |> Enum.filter(&(&1 in character.missing_limbs))
      |> length

    worn_on_max(item) - missing_limbs
  end

  def equip_item(%Character{} = character, %{worn_on: worn_on} = item, persist \\ true) do
    %{inventory: inventory, equipment: equipment} = character

    cond do
      is_nil(worn_on) ->
        false

      Item.too_powerful_for_character?(character, item) ->
        false

      !Item.useable_by_character?(character, item) ->
        false

      item.unfinished ->
        false

      :else ->
        cond do
          Enum.count(equipment, &(&1.worn_on == worn_on)) >= worn_on_max(character, item) ->
            item_to_remove =
              equipment
              |> Enum.find(&(&1.worn_on == worn_on))

            if item_to_remove do
              equipment = List.delete(equipment, item_to_remove)

              inventory = List.delete(inventory, item)

              if persist do
                %ItemInstance{id: item_to_remove.instance_id}
                |> Ecto.Changeset.change(%{equipped: false, class_id: nil})
                |> Repo.update!()
              end

              item_to_remove = Map.put(item_to_remove, :equipped, false)

              inventory = List.insert_at(inventory, -1, item_to_remove)

              if persist do
                %ItemInstance{id: item.instance_id}
                |> Ecto.Changeset.change(%{equipped: true, class_id: character.class_id})
                |> Repo.update!()
              end

              item = Map.put(item, :equipped, true)

              equipment = List.insert_at(equipment, -1, item)

              character =
                character
                |> Map.put(:inventory, inventory)
                |> Map.put(:equipment, equipment)
                |> Character.add_equipped_items_effects()
                |> Character.load_abilities()

              %{equipped: item, unequipped: [item_to_remove], character: character}
            else
              false
            end

          items_to_remove = conflicting_items(item, equipment) ->
            equipment = Enum.reject(equipment, &(&1 in items_to_remove))

            inventory = List.delete(inventory, item)

            if persist do
              Enum.each(items_to_remove, fn item_to_remove ->
                %ItemInstance{id: item_to_remove.instance_id}
                |> Ecto.Changeset.change(%{equipped: false, class_id: nil})
                |> Repo.update!()
              end)
            end

            inventory =
              items_to_remove
              |> Enum.reduce(inventory, fn item_to_remove, inv ->
                item_to_remove = Map.put(item_to_remove, :equipped, false)
                List.insert_at(inv, -1, item_to_remove)
              end)

            if persist do
              %ItemInstance{id: item.instance_id}
              |> Ecto.Changeset.change(%{equipped: true, class_id: character.class_id})
              |> Repo.update!()
            end

            item = Map.put(item, :equipped, false)

            equipment = List.insert_at(equipment, -1, item)

            character =
              character
              |> Map.put(:inventory, inventory)
              |> Map.put(:equipment, equipment)
              |> Character.add_equipped_items_effects()
              |> Character.load_abilities()

            %{equipped: item, unequipped: items_to_remove, character: character}

          true ->
            inventory =
              inventory
              |> List.delete(item)

            if persist do
              %ItemInstance{id: item.instance_id}
              |> Ecto.Changeset.change(%{equipped: true, class_id: character.class_id})
              |> Repo.update!()
            end

            item = Map.put(item, :equipped, true)

            equipment =
              equipment
              |> List.insert_at(-1, item)

            character =
              character
              |> Map.put(:inventory, inventory)
              |> Map.put(:equipment, equipment)
              |> Character.add_equipped_items_effects()
              |> Character.load_abilities()

            %{equipped: item, character: character}
        end
    end
  end

  defp conflicting_worn_on("Held"), do: ["Two Handed"]
  defp conflicting_worn_on("Two Handed"), do: ["Held"]
  defp conflicting_worn_on(_), do: []

  defp conflicting_items(item, equipment) do
    Enum.filter(equipment, fn equipped_item ->
      equipped_item.worn_on in conflicting_worn_on(item.worn_on) or
        (!is_nil(Item.enchantment(item)) and
           Item.enchantment(item) == Item.enchantment(equipped_item))
    end)
    |> case do
      [] ->
        nil

      conflicts ->
        conflicts
    end
  end
end
