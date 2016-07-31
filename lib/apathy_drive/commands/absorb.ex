defmodule ApathyDrive.Commands.Absorb do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["absorb", "disintegrate"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Absorb what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{spirit: %Spirit{inventory: inventory}} = mobile, arguments) do
    item_name = Enum.join(arguments, " ")

    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
        Mobile.send_scroll(mobile, "<p>You don't see \"#{item_name}\" here.</p>")
        room
      %{item: item} ->
        mobile = put_in(mobile.spirit.inventory, List.delete(inventory, item))

        exp = ApathyDrive.Item.deconstruction_experience(item)

        Mobile.send_scroll(mobile, "<p>You disintegrate the #{item["name"]} and absorb #{exp} essence.</p>")
        mobile = Mobile.add_experience(mobile, exp)

        put_in(room.mobiles[mobile.ref], mobile)
    end
  end

end
