defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Item, ShopItem}

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    list(room, character)

    room
  end

  def list(%Room{shop: items}, character) when map_size(items) == 0 do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"
    )
  end

  def list(%Room{shop: items}, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Quantity</span>    <span class='dark-cyan'>Price</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
    )

    items
    |> Enum.each(fn {_name, %ShopItem{} = shop_item} ->
      if shop_item.stock > 0 do
        if shop_item.item.cost_value do
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(shop_item.item, pad_trailing: 30)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.stock), 12)
            }</span><span class='dark-cyan'>#{shop_item.item.cost_value} #{
              shop_item.item.cost_currency
            }s</span></p>"
          )
        else
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(shop_item.item, pad_trailing: 30)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.stock), 12)
            }</span><span class='dark-cyan'>FREE</span></p>"
          )
        end
      end
    end)
  end
end
