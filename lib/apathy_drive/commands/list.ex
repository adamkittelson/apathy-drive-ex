defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Item}

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    room
    |> Room.items_for_sale
    |> list(character)

    room
  end

  def list([], character) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
  end

  def list(items, character) do
    character
    |> Mobile.send_scroll("<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-cyan'>───────────────────────────────────────────────────────────────────────────</span></p>")

    items
    |> Enum.each(fn(%Item{name: name} = item) ->
         price = Item.price_for_character(item, character)
         price = if price > 0, do: "#{price} gold", else: "FREE"
         name = Item.colored_name(item, ljust: 30)

         Mobile.send_scroll(character, "<p>#{name}<span class='dark-cyan'>#{price}</span></p>")
       end)
  end
end
