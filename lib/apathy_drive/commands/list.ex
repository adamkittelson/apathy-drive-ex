defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterShop,
    Currency,
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

  def list(%Room{shop: %Shop{} = shop} = room, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Quantity</span>    <span class='dark-cyan'>Price</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
    )

    CharacterShop.restock!(room, character)

    IO.puts("done restocking!")

    items = CharacterShop.items(shop, character)

    items
    |> Enum.group_by(& &1.name)
    |> Enum.sort_by(
      fn {_name, items} ->
        Shop.buy_price(shop, character, List.first(items))
      end,
      &>=/2
    )
    |> Enum.each(fn {_name, [%Item{} = item | _rest] = items} ->
      padding = 30

      value =
        shop
        |> Shop.buy_price(character, item)
        |> Currency.set_value()
        |> Currency.to_string(:short)
        |> case do
          "" -> "FREE"
          value -> value
        end

      Mobile.send_scroll(
        character,
        "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{String.pad_trailing(items |> length() |> to_string(), 12)}</span><span class='dark-cyan'>#{value} #{Shop.item_disclaimer(item, character)}</span></p>"
      )
    end)

    shop.shop_items
    |> Enum.each(fn %ShopItem{} = shop_item ->
      if shop_item.count > 0 do
        shop_item = put_in(shop_item.item.level, character.level)
        item = Item.from_assoc(shop_item)

        padding = 30

        if item.cost_value do
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{String.pad_trailing(to_string(shop_item.count), 12)}</span><span class='dark-cyan'>#{trunc(item.cost_value * shop.cost_multiplier)} #{item.cost_currency}s #{Shop.item_disclaimer(item, character)}</span></p>"
          )
        else
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character)}<span class='dark-cyan'>#{String.pad_trailing(to_string(shop_item.count), 12)}</span><span class='dark-cyan'>FREE</span> #{Shop.item_disclaimer(item, character)}</p>"
          )
        end
      end
    end)
  end

  def list(%Room{shop: nil} = room, character) do
    if Trainer.trainer?(room) do
      character
      |> Mobile.send_scroll(
        "<p><span class='white'>The following abilities may be trained here:</span></p>"
      )
      |> Mobile.send_scroll(
        "<p><span class='dark-magenta'>Rank  Ability                  Requirements</span></p>"
      )

      room.trainable_skills
      |> Enum.sort_by(& &1.required_level)
      |> Enum.each(fn %Skill{} = skill ->
        name = String.pad_trailing(skill.name, 24)

        current_level = Skill.module(skill.name).skill_level(character)

        level =
          "#{current_level}/#{skill.max_level}"
          |> to_string()
          |> String.pad_trailing(5)

        prereq = Skill.module(skill.name).prereq() && Skill.module(skill.name).prereq().name()

        prereq =
          if prereq do
            prereq <> " Rank #{current_level + 1}"
          end

        prereq =
          ["Level #{skill.required_level}", prereq]
          |> Enum.reject(&is_nil/1)
          |> Enum.join(", ")

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{level} #{name} #{prereq}</span></p>"
        )
      end)
    else
      Mobile.send_scroll(
        character,
        "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"
      )
    end
  end
end
