defmodule ApathyDrive.Commands.Buy do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Mobile, Item, Repo}

  def keywords, do: ["buy"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    buy(room, Room.items_for_sale(room), character, Enum.join(arguments, " "))
  end

  def buy(%Room{} = room, [], character, _item_name) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>")
    room
  end

  def buy(%Room{} = room, items, character, item_name) do
    case Match.all(items, :name_contains, item_name) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{item_name}\" does not appear to be for sale here.</p>")
        room
      %Item{} = item ->
        price =
          item
          |> Map.put(:level, character.level)
          |> Item.price

        if price > character.gold do
          Mobile.send_scroll(character, "<p>You cannot afford to buy #{Item.colored_name(item)}.</p>")
          room
        else
          Room.update_mobile(room, character.ref, fn(char) ->

            Item.generate_for_character!(item, character, :purchased)

            update_in(char.gold, &(&1 - price))
            |> Character.load_items
            |> Repo.save!
            |> Mobile.send_scroll("<p>You purchase #{Item.colored_name(item)} for #{price} gold.</p>")
          end)
        end
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match)}</p>")
        end)
        room
    end
  end
end
