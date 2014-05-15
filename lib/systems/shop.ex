defmodule Systems.Shop do
  def list(character, room) do
    if ApathyDrive.Entity.has_component?(room, Components.Shop) do
      Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span></p>"])
      Components.Player.send_message(character, ["scroll", "<p><span class='dark-cyan'>─────────────────────────────────────────────────────────────────</span></p>"])
      Enum.each(Components.Shop.value(room), fn(item_hash) ->
        item_name = Components.find_by(Components.ID, item_hash["item"]) |> Components.Name.value
        cost = case item_hash["cost"] do
          nil ->
            "Free"
          amount when amount == 1 ->
            "#{amount} #{item_hash["denomination"]}"
          amount ->
            "#{amount} #{item_hash["denomination"]}s"
        end
        Components.Player.send_message(character, ["scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 30)}</span><span class='dark-cyan'>#{cost}</span></p>"])
      end)
    else
      Components.Player.send_message(character, ["scroll", "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>"])
    end
  end

  def buy(character, room, item) do
    if ApathyDrive.Entity.has_component?(room, Components.Shop) do
      case Systems.Match.all(Components.Shop.items(room), :name_contains, item) do
        [match] ->
          Systems.Item.spawn_item(match, character)
          Components.Player.send_message(character, ["scroll", "<p>You just bought #{Components.Name.value(match)} for nothing.</p>"])
        [] ->
          Components.Player.send_message(character, ["scroll", "<p>\"#{item}\" does not appear to be for sale here.</p>"])
        matches ->
          match_names = matches |> Enum.map &(Components.Name.value(&1))
          Components.Player.send_message(character, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
          Enum.each match_names, fn(match_name) ->
            Components.Player.send_message(character, ["scroll", "<p>-- #{match_name}</p>"])
          end
      end
    else
      Components.Player.send_message(character, ["scroll", "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>"])
    end
  end

  def sell(character, room, item) do
    if ApathyDrive.Entity.has_component?(room, Components.Shop) do
      case Systems.Match.all(Components.Items.get_items(character), :name_contains, item) do
        [match] ->
          Components.Items.remove_item(character, match)
          ApathyDrive.Entity.save!(character)
          Components.Player.send_message(character, ["scroll", "<p>You just sold #{Components.Name.value(match)} for nothing.</p>"])
          ApathyDrive.Entity.delete!(match)
        [] ->
          Components.Player.send_message(character, ["scroll", "<p>You don't have \"#{item}\" to sell!</p>"])
        matches ->
          match_names = matches |> Enum.map &(Components.Name.value(&1))
          Components.Player.send_message(character, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
          Enum.each match_names, fn(match_name) ->
            Components.Player.send_message(character, ["scroll", "<p>-- #{match_name}</p>"])
          end
      end
    else
      Components.Player.send_message(character, ["scroll", "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>"])
    end
  end
end
