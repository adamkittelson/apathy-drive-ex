defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Item, Shop, ShopItem}

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    list(room, character)

    room
  end

  def list(%Room{shop: %Shop{shop_items: items, cost_multiplier: multiplier}}, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Quantity</span>    <span class='dark-cyan'>Price</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
    )

    items
    |> Enum.each(fn %ShopItem{} = shop_item ->
      if shop_item.count > 0 do
        if shop_item.item.cost_value do
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(shop_item.item, pad_trailing: 30)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.count), 12)
            }</span><span class='dark-cyan'>#{trunc(shop_item.item.cost_value * multiplier)} #{
              shop_item.item.cost_currency
            }s #{Shop.item_disclaimer(shop_item.item, character)}</span></p>"
          )
        else
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(shop_item.item, pad_trailing: 30)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.count), 12)
            }</span><span class='dark-cyan'>FREE</span> #{
              Shop.item_disclaimer(shop_item.item, character)
            }</p>"
          )
        end
      end
    end)
  end

  def list(%Room{shop: nil}, character) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"
    )
  end
end
