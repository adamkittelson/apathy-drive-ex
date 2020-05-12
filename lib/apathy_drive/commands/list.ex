defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterClass,
    Class,
    Mobile,
    Item,
    Repo,
    Shop,
    ShopItem,
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
    if Trainer.trainer?(room) && room.trainer_id && room.trainer.class_id do
      class = Repo.get(Class, room.trainer.class_id)

      character_class =
        Repo.get_by(CharacterClass, character_id: character.id, class_id: room.trainer.class_id) ||
          %CharacterClass{level: 0, character_id: character.id}

      character
      |> Mobile.send_scroll("<p>                <span class='white'>#{class.name}</span></p>")

      character
      |> Mobile.send_scroll(
        "<p><span class='dark-magenta'>Level</span>   <span class='dark-magenta'>Exp Cost</span>       <span class='dark-magenta'>Abilities</span></p>"
      )
      |> Mobile.send_scroll(
        "<p><span class='dark-cyan'>------------------------------------------------------</span></p>"
      )

      range = (character_class.level + 1)..(character_class.level + 6)

      Enum.each(range, fn level ->
        abilities =
          ApathyDrive.ClassAbility
          |> Ecto.Query.where(
            [ss],
            ss.class_id == ^class.id and ss.level == ^level and ss.auto_learn == true
          )
          |> Ecto.Query.preload([:ability])
          |> Repo.all()
          |> Enum.map(& &1.ability.name)
          |> ApathyDrive.Commands.Inventory.to_sentence()

        exp =
          character
          |> ApathyDrive.Commands.Train.required_experience(class.id, level)
          |> to_string()
          |> String.pad_trailing(15)

        level =
          level
          |> to_string()
          |> String.pad_trailing(8)

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{level}</span><span class='dark-cyan'>#{exp}</span><span class='dark-cyan'>#{
            abilities
          }</span></p>"
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
