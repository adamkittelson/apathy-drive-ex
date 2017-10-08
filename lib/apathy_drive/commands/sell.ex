defmodule ApathyDrive.Commands.Sell do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Enchantment, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["sell"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    sell(room, Room.items_for_sale(room), character, Enum.join(arguments, " "))
  end

  def sell(%Room{} = room, [], character, _item_name) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
    room
  end

  def sell(%Room{} = room, shop_items, %Character{ref: ref} = character, "all") do
    character.inventory
    |> Enum.reject(&upgrade?(&1, character))
    |> Enum.reject(&enchanted?(&1))
    |> Enum.reduce(room, fn(item, updated_room) ->
         character = updated_room.mobiles[ref]
         sell(updated_room, shop_items, character, item)
       end)
  end

  def sell(%Room{} = room, _shop_items, character, %Item{instance_id: instance_id} = item) do
    case Item.price(item) do
      "priceless" ->
        Mobile.send_scroll(character, "<p><span class='red'>#{Item.colored_name(item)} is a priceless artifact and cannot be sold!</span></p>")
       price ->
         enchantment_time = Enchantment.enchantment_time(item)
         price =
           if enchantment_time > 0 do
             price + div(enchantment_time, 60)
           else
            div(price, 10)
           end
         Repo.delete!(%ItemInstance{id: instance_id})

         Room.update_mobile(room, character.ref, fn(char) ->
           update_in(char.gold, &(&1 + price))
           |> Character.load_items
           |> Repo.save!
           |> Mobile.send_scroll("<p>You sold #{Item.colored_name(item)} for #{price} gold.</p>")
         end)
    end
  end

  def sell(%Room{} = room, shop_items, character, item_name) do
    character.inventory
    |> Match.all(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to sell!</p>")
           room
         %Item{} = item ->
           sell(room, shop_items, character, item)
         matches ->
           if Enum.all?(matches, & &1.name == List.first(matches).name) do
             sell(room, shop_items, character, List.first(matches))
           else
             Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
             Enum.each(matches, fn(match) ->
               Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match)}</p>")
             end)
             room
           end
       end
  end

  defp upgrade?(%Item{} = item, %Character{} = character) do
    %{character: with_item} = ApathyDrive.Commands.Wear.equip_item(character, item, false)

    Mobile.power_at_level(with_item, character.level) > Mobile.power_at_level(character, character.level)
  end

  def enchanted?(%Item{instance_id: nil}), do: true
  def enchanted?(%Item{instance_id: id}) do
    require Ecto.Query
    Enchantment
    |> Ecto.Query.where([e], e.items_instances_id == ^id)
    |> Repo.all
    |> Enum.any?
  end
end
