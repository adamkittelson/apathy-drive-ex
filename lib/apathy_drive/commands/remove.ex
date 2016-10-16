defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Remove what?</p>")
    room
  end
  def execute(%Room{} = room, %Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}} = monster, arguments) do
    item = Enum.join(arguments, " ")

    monster =
      equipment
      |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
      |> Match.one(:name_contains, item)
      |> case do
           nil ->
             Monster.send_scroll(monster, "<p>You don't have \"#{item}\" equipped.</p>")
             monster
           %{item: item_to_remove} ->
             equipment =
               equipment
               |> List.delete(item_to_remove)

             inventory =
               inventory
               |> List.insert_at(-1, item_to_remove)

             monster = put_in(monster.spirit.inventory, inventory)
             monster = put_in(monster.spirit.equipment, equipment)
                      |> Monster.set_abilities
                      |> Monster.set_max_mana
                      |> Monster.set_mana
                      |> Monster.set_max_hp
                      |> Monster.set_hp
                      |> Monster.save

             Monster.send_scroll(monster, "<p>You remove #{item_to_remove["name"]}.</p>")
             monster
         end

    put_in room.monsters[monster.ref], monster
  end

end
