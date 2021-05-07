defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    Mobile,
    Item,
    Shop,
    ShopItem,
    Skill,
    Trainer
  }

  require Ecto.Query

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
        shop_item = put_in(shop_item.item.level, character.level)
        item = Item.from_assoc(shop_item)

        padding = 30

        if item.cost_value do
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.count), 12)
            }</span><span class='dark-cyan'>#{trunc(item.cost_value * multiplier)} #{
              item.cost_currency
            }s #{Shop.item_disclaimer(item, character)}</span></p>"
          )
        else
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{
              String.pad_trailing(to_string(shop_item.count), 12)
            }</span><span class='dark-cyan'>FREE</span> #{Shop.item_disclaimer(item, character)}</p>"
          )
        end
      end
    end)
  end

  def list(%Room{shop: nil} = room, character) do
    if Trainer.trainer?(room) do
      Enum.each(room.trainable_skills, fn %Skill{name: skill} ->
        Mobile.send_scroll(character, "<p>#{skill}</p>")
      end)
    else
      Mobile.send_scroll(
        character,
        "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"
      )
    end
  end
end
