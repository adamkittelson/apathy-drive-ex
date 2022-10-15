defmodule ApathyDrive.Commands.Sell do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    Currency,
    Enchantment,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Repo,
    Shop
  }

  require Ecto.Query

  def keywords, do: ["sell"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    if String.trim(item_name) == "" do
      Mobile.send_scroll(
        character,
        "<p><span class='red'>Syntax: SELL {item}</span></p>"
      )

      room
    else
      sell(room, character, item_name)
    end
  end

  def sell(%Room{shop: nil} = room, character, _args) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>"
    )

    room
  end

  def sell(%Room{} = room, %Character{ref: ref} = character, "all") do
    items_to_sell =
      character.inventory
      |> Enum.reject(&upgrade?(&1, character))
      |> Enum.reject(&enchanted?(&1))

    if Enum.any?(items_to_sell) do
      items_to_sell
      |> Enum.reduce(room, fn item, updated_room ->
        character = updated_room.mobiles[ref]
        sell(updated_room, character, item)
      end)
    else
      Mobile.send_scroll(
        character,
        "<p>All your items are too valuable to auto-sell and must be sold one at a time.</p>"
      )

      room
    end
  end

  def sell(%Room{} = room, character, %Item{instance_id: instance_id} = item) do
    cost_in_copper = Shop.sell_price(room.shop, character, item)

    if item.unfinished do
      Mobile.send_scroll(
        character,
        "<p>You may not sell unfinished items!</p>"
      )

      room
    else
      Room.update_mobile(room, character.ref, fn _room, char ->
        ItemInstance
        |> Repo.get!(instance_id)
        |> Repo.delete!()

        enchantment =
          Enchantment
          |> Ecto.Query.where([e], e.items_instances_id == ^instance_id and e.finished == true)
          |> Repo.one()

        if enchantment do
          Repo.delete!(enchantment)
        else
          0
        end

        currency = Currency.set_value(cost_in_copper)
        char_currency = Currency.add(char, cost_in_copper)

        if cost_in_copper == 0 do
          Mobile.send_scroll(
            char,
            "<p>You sold #{Item.colored_name(item, character: char)} for nothing.</p>"
          )

          char_ref = char.ref

          Enum.each(room.mobiles, fn
            {ref, %Character{} = mobile} when ref != char_ref ->
              Mobile.send_scroll(
                mobile,
                "<p>#{Mobile.colored_name(char)} sold #{Item.colored_name(item, character: mobile)} for nothing.</p>"
              )

            _ ->
              :noop
          end)
        else
          Mobile.send_scroll(
            char,
            "<p>You sold #{Item.colored_name(item, character: char)} for #{Currency.to_string(currency)}.</p>"
          )

          char_ref = char.ref

          Enum.each(room.mobiles, fn
            {ref, %Character{} = mobile} when ref != char_ref ->
              Mobile.send_scroll(
                mobile,
                "<p>#{Mobile.colored_name(char)} sold #{Item.colored_name(item, character: mobile)} for #{Currency.to_string(currency)}.</p>"
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
      end)
    end
  end

  def sell(%Room{} = room, character, item_name) do
    character.inventory
    |> Match.all(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to sell!</p>")
        room

      %Item{} = item ->
        sell(room, character, item)

      matches ->
        if Enum.all?(matches, &(&1.name == List.first(matches).name)) do
          sell(room, character, List.first(matches))
        else
          Mobile.send_scroll(
            character,
            "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
          )

          Enum.each(matches, fn match ->
            Mobile.send_scroll(
              character,
              "<p>-- #{Item.colored_name(match, character: character)}</p>"
            )
          end)

          room
        end
    end
  end

  defp upgrade?(%Item{} = item, %Character{} = character) do
    %{character: with_item} = ApathyDrive.Commands.Wear.equip_item(character, item, false)

    Mobile.power(with_item) > Mobile.power(character)
  end

  def enchanted?(%Item{instance_id: nil}), do: false

  def enchanted?(%Item{instance_id: id}) do
    require Ecto.Query

    Enchantment
    |> Ecto.Query.where([e], e.items_instances_id == ^id)
    |> Repo.all()
    |> Enum.any?()
  end
end
