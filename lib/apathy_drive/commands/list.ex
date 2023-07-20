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

  def list(%Room{shop: %Shop{} = shop} = _room, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Quantity</span>    <span class='dark-cyan'>Price</span></p>"
    )
    |> Mobile.send_scroll(
      "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
    )

    # CharacterShop.restock!(room, character)

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
        "<p>#{Item.colored_name(item, pad_trailing: padding, character: character, shop_item: true)}<span class='dark-cyan'>#{String.pad_trailing(items |> length() |> to_string(), 12)}</span><span class='dark-cyan'>#{value} #{Shop.item_disclaimer(item, character)}</span></p>"
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
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character, shop_item: true)}<span class='dark-cyan'>#{String.pad_trailing(to_string(shop_item.count), 12)}</span><span class='dark-cyan'>#{trunc(item.cost_value * shop.cost_multiplier)} #{item.cost_currency}s #{Shop.item_disclaimer(item, character)}</span></p>"
          )
        else
          Mobile.send_scroll(
            character,
            "<p>#{Item.colored_name(item, pad_trailing: padding, character: character, shop_item: true)}<span class='dark-cyan'>#{String.pad_trailing(to_string(shop_item.count), 12)}</span><span class='dark-cyan'>FREE</span> #{Shop.item_disclaimer(item, character)}</p>"
          )
        end
      end
    end)
  end

  def list(%Room{shop: nil} = room, character) do
    if Trainer.ability_trainer?(room), do: list_abilities(room, character)

    if Trainer.skill_trainer?(room), do: list_skills(room, character)

    if !Trainer.skill_trainer?(room) and !Trainer.ability_trainer?(room) do
      Mobile.send_scroll(
        character,
        "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"
      )
    end
  end

  def list_skills(room, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-magenta'>-=-=-=-=-=-=-=-=-=-=-=-=-=  <span class='white'>Skill Listing</span>  <span class='dark-magenta'>=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span></p>"
    )

    room.trainable_skills
    |> Enum.filter(&(&1.skill.type == "skill"))
    |> Enum.group_by(& &1.skill.required_level)
    |> Enum.sort()
    |> Enum.each(fn {level, list} ->
      character
      |> Mobile.send_scroll(
        "<p><span class='dark-cyan'>Level #{String.pad_leading(to_string(level), 2)}</span> <span class='dark-magenta'>-------------------------</span> <span class='dark-cyan'>Cost</span> <span class='dark-magenta'>------------------------</span> <span class='dark-cyan'>Rating</span></p>"
      )

      list
      |> Enum.sort_by(& &1.skill.name)
      |> Enum.each(fn %{class_id: _, skill: %Skill{} = skill} ->
        name = String.pad_trailing(skill.name, 30)

        cost = Trainer.dev_cost(character, skill)

        string =
          cost
          |> to_string()
          |> String.pad_trailing(28)

        cost =
          if Character.development_points(character) > cost do
            "<span class='green'>#{string}</span>"
          else
            "<span class='dark-red'>#{string}</span>"
          end

        rating =
          Skill.module(skill.name).skill_level(character)
          |> to_string()
          |> String.pad_leading(3)

        Mobile.send_scroll(
          character,
          "<p>     #{name}#{cost}#{rating}%</span></p>"
        )
      end)
    end)

    character
    |> Mobile.send_scroll(
      "<p><span class='dark-magenta'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span></p>"
    )
  end

  def list_abilities(room, character) do
    character
    |> Mobile.send_scroll(
      "<p><span class='dark-magenta'>-=-=-=-=-=-=-=-=-=-=-=-=-=  <span class='white'>Ability Listing</span>  <span class='dark-magenta'>=-=-=-=-=-=-=-=-=-=-=-=-=-</span></p>"
    )

    room.trainable_skills
    |> Enum.filter(&(&1.skill.type == "ability"))
    |> Enum.group_by(& &1.skill.required_level)
    |> Enum.sort()
    |> Enum.each(fn {level, list} ->
      character
      |> Mobile.send_scroll(
        "<p><span class='dark-cyan'>Level #{String.pad_leading(to_string(level), 2)}</span> <span class='dark-magenta'>-------------------------</span> <span class='dark-cyan'>Skill</span> <span class='dark-magenta'>---------------</span> <span class='dark-cyan'>Cost</span> <span class='dark-magenta'>---</span> <span class='dark-cyan'>Power</span></p>"
      )

      list
      |> Enum.sort_by(& &1.skill.name)
      |> Enum.each(fn %{class_id: _, skill: %Skill{} = skill} ->
        name = String.pad_trailing(skill.name, 30)

        cost = Trainer.dev_cost(character, skill)

        string =
          cost
          |> to_string()
          |> String.pad_trailing(9)

        cost =
          if Character.development_points(character) > cost do
            "<span class='green'>#{string}</span>"
          else
            "<span class='dark-red'>#{string}</span>"
          end

        power = Skill.module(skill.name).skill_level(character)

        skill =
          (Skill.module(skill.name).casting_skill && Skill.module(skill.name).casting_skill.name)
          |> String.pad_trailing(22)

        Mobile.send_scroll(
          character,
          "<p>     #{name}#{skill}#{cost}*#{power}</span></p>"
        )
      end)
    end)

    character
    |> Mobile.send_scroll(
      "<p><span class='dark-magenta'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span></p>"
    )
  end
end
