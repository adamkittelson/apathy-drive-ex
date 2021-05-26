defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterShop,
    Currency,
    Mobile,
    Item,
    Shop,
    Skill,
    Trainer
  }

  require Ecto.Query

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    list(room, character)

    room
  end

  def list(%Room{shop: %Shop{} = shop} = room, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-green'>Item</span>                              <span class='dark-cyan'>Price</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
    )

    CharacterShop.restock!(room, character)

    IO.puts("done restocking!")

    items = CharacterShop.items(shop, character)

    items
    |> Enum.each(fn %Item{} = item ->
      padding = 30

      value =
        shop
        |> Shop.buy_price(character, item)
        |> Currency.set_value()
        |> Currency.to_string()
        |> case do
          "" -> "FREE"
          value -> value
        end

      Mobile.send_scroll(
        character,
        "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{value} #{Shop.item_disclaimer(item, character)}</span></p>"
      )
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
