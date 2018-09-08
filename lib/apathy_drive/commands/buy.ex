defmodule ApathyDrive.Commands.Buy do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Mobile, Item, Repo, Shop, ShopItem}

  def keywords, do: ["buy"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    buy(room, room.shop.shop_items, character, Enum.join(arguments, " "))
  end

  def buy(%Room{} = room, [], character, _item_name) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>"
    )

    room
  end

  def buy(%Room{} = room, items, character, item_name) do
    case Match.all(items, :name_contains, item_name) do
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
        # if price > character.gold do
        #   Mobile.send_scroll(
        #     character,
        #     "<p>You cannot afford to buy #{Item.colored_name(item)}.</p>"
        #   )

        #   room
        # else

        price_in_copper = Shop.buy_price(room.shop, character, shop_item.item)

        item_instance =
          room.shop.item_instances
          |> Enum.find(&(&1.item_id == shop_item.item_id))

        Room.update_mobile(room, character.ref, fn char ->
          item_instance
          |> Ecto.Changeset.change(%{
            shop_id: nil,
            character_id: char.id,
            equipped: false,
            hidden: false
          })
          |> Repo.update!()

          # update_in(char.gold, &(&1 - price))
          char
          |> Character.load_items()
          |> Repo.save!()
          |> Mobile.send_scroll(
            "<p>You purchase #{Item.colored_name(item_instance.item)} for #{price_in_copper} copper.</p>"
          )
        end)
        |> Shop.load()

      # end

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match)}</p>")
        end)

        room
    end
  end
end
