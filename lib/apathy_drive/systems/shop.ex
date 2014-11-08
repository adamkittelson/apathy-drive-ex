defmodule Systems.Shop do
  use Systems.Reload
  import Utility

  def list(spirit, nil, room) do
    send_message(spirit, "scroll", "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-cyan'>─────────────────────────────────────────────────────────────────</span></p>")
    Enum.each(Components.Shop.value(room), fn(item_hash) ->
      item_name = ItemTemplates.find_by_id(item_hash["item"]) |> Components.Name.value
      cost = case item_hash["cost"] do
        nil ->
          "Free"
        amount when amount == 1 ->
          "#{amount} #{item_hash["denomination"]}"
        amount ->
          "#{amount} #{item_hash["denomination"]}s"
      end
      send_message(spirit, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 30)}</span><span class='dark-cyan'>#{String.ljust(cost, 30)}</span></p>")
    end)
  end

  def list(spirit, monster, room) do
    send_message(spirit, "scroll", "<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span>                    <span class='dark-cyan'>Skill too low</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-cyan'>───────────────────────────────────────────────────────────────────────────</span></p>")
    Enum.each(Components.Shop.value(room), fn(item_hash) ->
      it = ItemTemplates.find_by_id(item_hash["item"])
      item_name = it |> Components.Name.value
      cost = case item_hash["cost"] do
        nil ->
          "Free"
        amount when amount == 1 ->
          "#{amount} #{item_hash["denomination"]}"
        amount ->
          "#{amount} #{item_hash["denomination"]}s"
      end

      skill_too_low = Systems.Item.skill_too_low(monster, it)

      if skill_too_low do
        send_message(spirit, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 30)}</span><span class='dark-cyan'>#{String.ljust(cost, 25)}</span><span class='dark-cyan'>#{skill_too_low}</span></p>")
      else
        send_message(spirit, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 30)}</span><span class='dark-cyan'>#{cost}</span></p>")
      end
    end)
  end

  def buy(character, room, item) do
    cond do
      !Entity.has_component?(room, Components.Shop) ->
        send_message(character, "scroll", "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>")
      true ->
        case Systems.Match.all(Components.Shop.items(room), :name_contains, item) do
          [match] ->
            Systems.Item.spawn_item(match, character)
            send_message(character, "scroll", "<p>You just bought #{Components.Name.value(match)} for nothing.</p>")
          [] ->
            send_message(character, "scroll", "<p>\"#{item}\" does not appear to be for sale here.</p>")
          matches ->
            match_names = matches |> Enum.map &(Components.Name.value(&1))
            send_message(character, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
            Enum.each match_names, fn(match_name) ->
              send_message(character, "scroll", "<p>-- #{match_name}</p>")
            end
        end
    end
  end

  def sell(character, room, item) do
    cond do
      !Entity.has_component?(room, Components.Shop) ->
        send_message(character, "scroll", "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
      true ->
        case Systems.Match.all(Components.Items.get_items(character), :name_contains, item) do
          [match] ->
            Components.Items.remove_item(character, match)
            Entities.save(character)
            send_message(character, "scroll", "<p>You just sold #{Components.Name.value(match)} for nothing.</p>")
            Entities.delete!(match)
          [] ->
            send_message(character, "scroll", "<p>You don't have \"#{item}\" to sell!</p>")
          matches ->
            match_names = matches |> Enum.map &(Components.Name.value(&1))
            send_message(character, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
            Enum.each match_names, fn(match_name) ->
              send_message(character, "scroll", "<p>-- #{match_name}</p>")
            end
        end
    end
  end
end
