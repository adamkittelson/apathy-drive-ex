defmodule ApathyDrive.Commands.Absorb do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["absorb", "disintegrate"]

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Absorb what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{spirit: %Spirit{inventory: inventory}} = monster, arguments) do
    item_name = Enum.join(arguments, " ")

    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
        Monster.send_scroll(monster, "<p>You don't see \"#{item_name}\" here.</p>")
        room
      %{item: item} ->
        monster = put_in(monster.spirit.inventory, List.delete(inventory, item))

        exp = ApathyDrive.Item.deconstruction_experience(item)

        Monster.send_scroll(monster, "<p>You disintegrate the #{item["name"]} and absorb #{exp} essence.</p>")
        monster = Monster.add_experience(monster, exp)

        put_in(room.monsters[monster.ref], monster)
    end
  end

end
