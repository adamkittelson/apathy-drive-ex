defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Remove what?</p>")
    room
  end
  def execute(%Room{} = room, %Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, arguments) do
    item = Enum.join(arguments, " ")

    mobile =
      equipment
      |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
      |> Match.one(:name_contains, item)
      |> case do
           nil ->
             Mobile.send_scroll(mobile, "<p>You don't have \"#{item}\" equipped.</p>")
             mobile
           %{item: item_to_remove} ->
             equipment =
               equipment
               |> List.delete(item_to_remove)

             inventory =
               inventory
               |> List.insert_at(-1, item_to_remove)

             mobile = put_in(mobile.spirit.inventory, inventory)
             mobile = put_in(mobile.spirit.equipment, equipment)
                      |> Mobile.set_abilities
                      |> Mobile.set_max_mana
                      |> Mobile.set_mana
                      |> Mobile.set_max_hp
                      |> Mobile.set_hp
                      |> Mobile.save

             Mobile.send_scroll(mobile, "<p>You remove #{item_to_remove["name"]}.</p>")
             mobile
         end

    put_in room.mobiles[mobile.ref], mobile
  end

end
