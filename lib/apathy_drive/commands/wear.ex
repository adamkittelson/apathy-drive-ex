defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Equip what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{ref: ref, spirit: %Spirit{inventory: inventory}}, ["all"]) do
    inventory
    |> Enum.map(&(&1["name"]))
    |> Enum.reduce(room, fn(item_name, updated_room) ->
         mobile = updated_room.mobiles[ref]
         execute(updated_room, mobile, [item_name])
       end)
  end

  def execute(%Room{} = room, %Mobile{spirit: %Spirit{inventory: inventory}} = mobile, args) do
    item_name = Enum.join(args, " ")
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
       Mobile.send_scroll(mobile, "<p>You don't have \"#{item_name}\" left unequipped.</p>")
       room
     %{item: item} ->
       mobile =
         case equip_item(mobile, item) do
           %{equipped: equipped, unequipped: unequipped, mobile: mobile} ->
             Enum.each(unequipped, fn(item) ->
               Mobile.send_scroll(mobile, "<p>You remove #{item["name"]}.</p>")
             end)
             Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
             Mobile.save(mobile)
             mobile
           %{equipped: equipped, mobile: mobile} ->
             Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
             mobile
         end

       put_in(room.mobiles[mobile.ref], mobile)
    end
  end

  def equip_item(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, %{"worn_on" => worn_on} = item) do
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

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> Mobile.set_abilities
                   |> Mobile.set_max_mana
                   |> Mobile.set_mana
                   |> Mobile.set_max_hp
                   |> Mobile.set_hp

        %{equipped: item, unequipped: [item_to_remove], mobile: mobile}
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

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> Mobile.set_abilities
                   |> Mobile.set_max_mana
                   |> Mobile.set_mana
                   |> Mobile.set_max_hp
                   |> Mobile.set_hp

        %{equipped: item, unequipped: items_to_remove, mobile: mobile}
      true ->
        equipment =
          equipment
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.delete(item)

        mobile = put_in(mobile.spirit.inventory, inventory)
        mobile = put_in(mobile.spirit.equipment, equipment)
                 |> Mobile.set_abilities
                 |> Mobile.set_max_mana
                 |> Mobile.set_mana
                 |> Mobile.set_max_hp
                 |> Mobile.set_hp

        %{equipped: item, mobile: mobile}
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
