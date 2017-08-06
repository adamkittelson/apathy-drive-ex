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
    |> Enum.each(fn(%Item{name: _name} = item) ->
        can_use? = if Character.can_equip_item?(character, item), do: "", else: " (You can't use)"
        item
        |> Map.put(:level, character.level)
        |> Item.price
        |> case do
            "priceless" ->
              :noop
            price when price > 0 ->
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item, pad_trailing: 30)}<span class='dark-cyan'>#{price} gold#{can_use?}</span></p>")
            _ ->
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item, pad_trailing: 30)}<span class='dark-cyan'>FREE</span>#{can_use?}</p>")
           end
       end)
  end
end
