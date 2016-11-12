defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, Mobile}

  def keywords, do: ["i", "inv", "inventory"]

  def execute(%Room{} = room, %Character{equipment: equipment, inventory: inventory} = character, _args) do

    if equipment |> Enum.any? do
      Mobile.send_scroll(character, "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")

      equipment
      |> Enum.each(fn(item) ->
           Mobile.send_scroll(character, "<p><span class='dark-green'>#{Item.colored_name(item, ljust: 23)}</span><span class='dark-cyan'>(#{item.worn_on})</span></p>")
         end)
      Mobile.send_scroll(character, "<br>")
    end

    item_names = inventory |> Enum.map(&(Item.colored_name(&1)))
    if item_names |> Enum.count > 0 do
      Mobile.send_scroll(character, "<p>You are carrying #{Enum.join(item_names, ", ")}</p>")
    else
      Mobile.send_scroll(character, "<p>You are carrying nothing.</p>")
    end

    Mobile.send_scroll(character, "<p><span class='dark-green'>Wealth:</span> <span class='dark-cyan'>#{character.gold} gold crowns</span></p>")

    room
  end

end
