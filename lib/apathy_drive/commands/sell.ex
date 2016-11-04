defmodule ApathyDrive.Commands.Sell do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Mobile, Item, Repo}

  def keywords, do: ["sell"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    sell(room, Room.items_for_sale(room), character, Enum.join(arguments, " "))
  end

  def sell(%Room{} = room, [], character, _item_name) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
    room
  end

  def sell(%Room{} = room, _shop_items, character, item_name) do
    character
    |> Character.inventory
    |> Enum.map(& %{name: &1.item.name, character_item: &1})
    |> Match.all(:name_contains, item_name)
    |> case do
         [] ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to sell!</p>")
           room
         [%{character_item: %{level: level, item: %Item{} = item} = character_item}] ->
           price = Item.sell_price(item, level)

           Repo.delete(character_item)

           Room.update_mobile(room, character.ref, fn(char) ->
             update_in(char.gold, &(&1 + price))
             |> Repo.preload([characters_items: :item], [force: true])
             |> Repo.save!
             |> Mobile.send_scroll("<p>You sold #{Item.colored_name(item)} for #{price} gold.</p>")
           end)
         matches ->
           Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
           Enum.each(matches, fn(match) ->
             Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match.character_item.item)}</p>")
           end)
           room
       end
  end
end
