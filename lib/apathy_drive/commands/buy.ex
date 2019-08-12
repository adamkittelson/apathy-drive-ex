defmodule ApathyDrive.Commands.Buy do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Match, Mobile, Item, Repo, Shop, ShopItem}

  def keywords, do: ["buy"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    if String.trim(item_name) == "" do
      Mobile.send_scroll(
        character,
        "<p><span class='red'>Syntax: BUY {item}</span></p>"
      )

      room
    else
      buy(room, character, item_name)
    end
  end

  def buy(%Room{shop: nil} = room, character, _args) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>"
    )

    room
  end

  def buy(%Room{} = room, character, item_name) do
    case Match.all(room.shop.shop_items, :name_contains, item_name) do
      nil ->
        Mobile.send_scroll(
          character,
          "<p>\"#{item_name}\" does not appear to be for sale here.</p>"
        )

        room

      %ShopItem{count: 0} = shop_item ->
        message = "<p><span class='red'>#{shop_item.name} is out of stock.</span></p>"

        Mobile.send_scroll(character, message)

        room

      %ShopItem{} = shop_item ->
        price_in_copper = Shop.buy_price(room.shop, character, shop_item.item)

        cond do
          shop_item.item.weight >
              Character.max_encumbrance(character) - Character.encumbrance(character) ->
            Mobile.send_scroll(
              character,
              "<p>You cannot carry that much!<p>"
            )

            room

          Currency.wealth(character) < price_in_copper ->
            Mobile.send_scroll(
              character,
              "<p>You cannot afford to buy #{Item.colored_name(shop_item.item)}.<p>"
            )

            room

          :else ->
            item_instance =
              room.shop.item_instances
              |> Enum.find(&(&1.item_id == shop_item.item_id))

            Room.update_mobile(room, character.ref, fn char ->
              item_instance
              |> Ecto.Changeset.change(%{
                shop_id: nil,
                character_id: char.id,
                equipped: false,
                hidden: false,
                level: char.level
              })
              |> Repo.update!()

              currency = Currency.set_value(price_in_copper)
              char_currency = Currency.subtract(char, price_in_copper)

              if price_in_copper == 0 do
                Mobile.send_scroll(
                  char,
                  "<p>You purchase #{Item.colored_name(item_instance.item)} for nothing.</p>"
                )
              else
                Mobile.send_scroll(
                  char,
                  "<p>You purchase #{Item.colored_name(item_instance.item)} for #{
                    Currency.to_string(currency)
                  }.</p>"
                )
              end

              char
              |> Ecto.Changeset.change(%{
                runic: char_currency.runic,
                platinum: char_currency.platinum,
                gold: char_currency.gold,
                silver: char_currency.silver,
                copper: char_currency.copper
              })
              |> Repo.update!()
              |> Character.load_items()
              |> Repo.save!()
            end)
            |> Shop.load()
        end

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match.item)}</p>")
        end)

        room
    end
  end
end
