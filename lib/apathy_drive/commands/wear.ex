defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Equip what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{ref: ref, spirit: %Spirit{inventory: inventory}}, ["all"]) do
    inventory
    |> Enum.map(&(&1["name"]))
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         monster = updated_room.monsters[ref]
         execute(updated_room, monster, [item_name])
       end)
  end

  def execute(%Room{} = room, %Monster{spirit: %Spirit{inventory: inventory}} = monster, args) do
    item_name = Enum.join(args, " ")
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
       Monster.send_scroll(monster, "<p>You don't have \"#{item_name}\" left unequipped.</p>")
       room
     %{item: item} ->
       monster =
         case equip_item(monster, item) do
           %{equipped: equipped, unequipped: unequipped, monster: monster} ->
             Enum.each(unequipped, fn(item) ->
               Monster.send_scroll(monster, "<p>You remove #{item["name"]}.</p>")
             end)
             Monster.send_scroll(monster, "<p>You are now wearing #{equipped["name"]}.</p>")
             Monster.save(monster)
             monster
           %{equipped: equipped, monster: monster} ->
             Monster.send_scroll(monster, "<p>You are now wearing #{equipped["name"]}.</p>")
             monster
         end

       put_in(room.monsters[monster.ref], monster)
    end
  end

  def equip_item(%Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}} = monster, %{"worn_on" => worn_on} = item) do
    cond do
      Enum.count(equipment, &(&1["worn_on"] == worn_on)) >= worn_on_max(item) ->
        item_to_remove =
          equipment
          |> Enum.find(&(&1["worn_on"] == worn_on))

        equipment =
          equipment
          |> List.delete(item_to_remove)
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.insert_at(-1, item_to_remove)
          |> List.delete(item)

          monster = put_in(monster.spirit.inventory, inventory)
          monster = put_in(monster.spirit.equipment, equipment)
                   |> Monster.set_abilities
                   |> Monster.set_max_mana
                   |> Monster.set_mana
                   |> Monster.set_max_hp
                   |> Monster.set_hp

        %{equipped: item, unequipped: [item_to_remove], monster: monster}
      conflicting_worn_on(worn_on) |> Enum.any? ->
        items_to_remove =
          equipment
          |> Enum.filter(&(&1["worn_on"] in conflicting_worn_on(worn_on)))

        equipment =
          equipment
          |> Enum.reject(&(&1 in items_to_remove))
          |> List.insert_at(-1, item)

        inventory =
          items_to_remove
          |> Enum.reduce(inventory, fn(item_to_remove, inv) ->
               List.insert_at(inv, -1, item_to_remove)
             end)
          |> List.delete(item)

          monster = put_in(monster.spirit.inventory, inventory)
          monster = put_in(monster.spirit.equipment, equipment)
                   |> Monster.set_abilities
                   |> Monster.set_max_mana
                   |> Monster.set_mana
                   |> Monster.set_max_hp
                   |> Monster.set_hp

        %{equipped: item, unequipped: items_to_remove, monster: monster}
      true ->
        equipment =
          equipment
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.delete(item)

        monster = put_in(monster.spirit.inventory, inventory)
        monster = put_in(monster.spirit.equipment, equipment)
                 |> Monster.set_abilities
                 |> Monster.set_max_mana
                 |> Monster.set_mana
                 |> Monster.set_max_hp
                 |> Monster.set_hp

        %{equipped: item, monster: monster}
    end
  end

  defp worn_on_max(%{"worn_on" => "Finger"}), do: 2
  defp worn_on_max(%{"worn_on" => "Wrist"}),  do: 2
  defp worn_on_max(%{"worn_on" => _}),        do: 1

  defp conflicting_worn_on("Weapon Hand"),     do: ["Two Handed"]
  defp conflicting_worn_on("Off-Hand"),   do: ["Two Handed"]
  defp conflicting_worn_on("Two Handed"), do: ["Weapon Hand", "Off-Hand"]
  defp conflicting_worn_on(_), do: []

end
