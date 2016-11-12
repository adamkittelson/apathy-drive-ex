defmodule ApathyDrive.Commands.Sell do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, EntityItem, Item, Match, Mobile, Repo}

  def keywords, do: ["sell"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    sell(room, Room.items_for_sale(room), character, Enum.join(arguments, " "))
  end

  def sell(%Room{} = room, [], character, _item_name) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
    room
  end

  def sell(%Room{} = room, _shop_items, character, item_name) do
    character.inventory
    |> Match.all(:name_contains, item_name)
    |> case do
         nil ->
           Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" to sell!</p>")
           room
         %Item{entities_items_id: entities_items_id} = item ->
           case Item.price(item) do
             "priceless" ->
               Mobile.send_scroll("<p><span class='red'>#{Item.colored_name(item)} is a priceless artifact and cannot be sold!</span></p>")
              price ->
                price = div(price, 10)
                Repo.delete!(%EntityItem{id: entities_items_id})

                Room.update_mobile(room, character.ref, fn(char) ->
                  update_in(char.gold, &(&1 + price))
                  |> Character.load_items
                  |> Repo.save!
                  |> Mobile.send_scroll("<p>You sold #{Item.colored_name(item)} for #{price} gold.</p>")
                end)
           end
         matches ->
           Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
           Enum.each(matches, fn(match) ->
             Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match.character_item)}</p>")
           end)
           room
       end
  end
end
