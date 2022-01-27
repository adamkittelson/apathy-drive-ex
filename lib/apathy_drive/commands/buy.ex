defmodule ApathyDrive.Commands.Buy do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterShop,
    Currency,
    Match,
    Mobile,
    Monster,
    Item,
    ItemInstance,
    Repo,
    Shop,
    ShopItem
  }

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
    items = CharacterShop.items(room.shop, character) ++ room.shop.shop_items

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
              "<p>You cannot afford to buy #{Item.colored_name(shop_item.item, character: character)}.<p>"
            )

            room

          :else ->
            item_instance =
              room.shop.item_instances
              |> Enum.find(&(&1.item_id == shop_item.item_id))

            quality = determine_item_quality(character, item_instance.item)

            Room.update_mobile(room, character.ref, fn _room, char ->
              %{min: min, max: max} = Item.ac_for_item(item_instance.item)

              item_instance =
                item_instance
                |> Ecto.Changeset.change(%{
                  shop_id: nil,
                  character_id: char.id,
                  equipped: false,
                  hidden: false,
                  quality: quality,
                  ac: Enum.random(min..max),
                  level: char.level,
                  name: item_instance.item.name
                })
                |> Repo.update!()
                |> update_in([Access.key!(:item)], fn _item ->
                  item_instance
                  |> Item.from_assoc()
                  |> Item.load_item_types()
                end)

              item = item_instance.item

              currency = Currency.set_value(price_in_copper)
              char_currency = Currency.subtract(char, price_in_copper)

              affix_level = min(item.quality_level, character.level)

              {prefixes, suffixes} = Monster.item_affixes(item_instance, affix_level)

              prefixes = Enum.reject(prefixes, &is_nil/1)
              suffixes = Enum.reject(suffixes, &is_nil/1)

              name = Monster.item_name(item_instance, prefixes, suffixes)

              item_instance =
                item_instance
                |> Ecto.Changeset.change(%{name: name})
                |> Repo.update!()

              item = Item.from_assoc(item_instance)

              if price_in_copper == 0 do
                Mobile.send_scroll(
                  char,
                  "<p>You purchase #{Item.colored_name(item, character: char)} for nothing.</p>"
                )

                char_ref = char.ref

                Enum.each(room.mobiles, fn
                  {ref, %Character{} = mobile} when ref != char_ref ->
                    Mobile.send_scroll(
                      mobile,
                      "<p>#{Mobile.colored_name(char)} purchases #{Item.colored_name(item, character: mobile)} for nothing.</p>"
                    )

                  _ ->
                    :noop
                end)
              else
                Mobile.send_scroll(
                  char,
                  "<p>You purchase #{Item.colored_name(item, character: char)} for #{Currency.to_string(currency)}.</p>"
                )

                char_ref = char.ref

                Enum.each(room.mobiles, fn
                  {ref, %Character{} = mobile} when ref != char_ref ->
                    Mobile.send_scroll(
                      mobile,
                      "<p>#{Mobile.colored_name(char)} purchases #{Item.colored_name(item, character: mobile)} for #{Currency.to_string(currency)}.</p>"
                    )

                  _ ->
                    :noop
                end)
              end

              if item.quality in ["magic", "rare", "unique"],
                do: ApathyDrive.Commands.Look.look_at_item(char, item)

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
            end)
            |> Shop.load()
        end

      %Item{} = item ->
        IO.puts("item")
        price_in_copper = Shop.buy_price(room.shop, character, item)

        cond do
          item.weight > Character.max_encumbrance(character) - Character.encumbrance(character) ->
            Mobile.send_scroll(
              character,
              "<p>You cannot carry that much!<p>"
            )

            room

          Currency.wealth(character) < price_in_copper ->
            Mobile.send_scroll(
              character,
              "<p>You cannot afford to buy #{Item.colored_name(item, character: character)}.<p>"
            )

            room

          :else ->
            item_instance = Repo.get(ItemInstance, item.instance_id)

            Room.update_mobile(room, character.ref, fn _room, char ->
              item_instance
              |> Ecto.Changeset.change(%{
                shop_id: nil,
                character_shop_id: nil,
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
                  "<p>You purchase #{Item.colored_name(item, character: char)} for nothing.</p>"
                )

                char_ref = char.ref

                Enum.each(room.mobiles, fn
                  {ref, %Character{} = mobile} when ref != char_ref ->
                    Mobile.send_scroll(
                      mobile,
                      "<p>#{Mobile.colored_name(char)} purchases #{Item.colored_name(item, character: mobile)} for nothing.</p>"
                    )

                  _ ->
                    :noop
                end)
              else
                Mobile.send_scroll(
                  char,
                  "<p>You purchase #{Item.colored_name(item, character: char)} for #{Currency.to_string(currency)}.</p>"
                )

                char_ref = char.ref

                Enum.each(room.mobiles, fn
                  {ref, %Character{} = mobile} when ref != char_ref ->
                    Mobile.send_scroll(
                      mobile,
                      "<p>#{Mobile.colored_name(char)} purchases #{Item.colored_name(item, character: mobile)} for #{Currency.to_string(currency)}.</p>"
                    )

                  _ ->
                    :noop
                end)
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
        end

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn
          %ShopItem{} = match ->
            Mobile.send_scroll(
              character,
              "<p>-- #{Item.colored_name(match.item, character: character)}</p>"
            )

          %Item{} = match ->
            Mobile.send_scroll(
              character,
              "<p>-- #{Item.colored_name(match, character: character)}</p>"
            )
        end)

        room
    end
  end

  def determine_item_quality(character, item) do
    magic_find =
      Mobile.ability_value(character, "MagicFind") +
        Mobile.attribute_at_level(character, :charm, character.level)

    cond do
      Monster.rare?(character.level, item.quality_level, magic_find) ->
        IO.puts("dropped rare!")
        "rare"

      Monster.magic?(character.level, item.quality_level, magic_find) ->
        IO.puts("dropped magic!")
        "magic"

      :else ->
        IO.puts("dropped normal!")
        "normal"
    end
  end
end
