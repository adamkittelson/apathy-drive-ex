defmodule Systems.Shop do
  use Systems.Reload
  import Utility
  import Systems.Text

  def list(%Spirit{} = spirit, %Room{shop_items: item_template_ids}) do
    spirit
    |> Spirit.send_scroll("<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price (Experience)</span></p>")
    |> Spirit.send_scroll("<p><span class='dark-cyan'>─────────────────────────────────────────────────────────────────</span></p>")

    item_template_ids
    |> Enum.map(&ItemTemplate.find/1)
    |> Enum.map(&ItemTemplate.value/1)
    |> Enum.each(fn(%ItemTemplate{name: name, cost: cost}) ->
      Spirit.send_scroll(spirit, "<p><span class='dark-green'>#{String.ljust(name, 30)}</span><span class='dark-cyan'>#{cost || "Free"}</span></p>")
    end)
    spirit
  end

  def list(%Monster{} = monster, %Room{shop_items: item_template_ids}) do
    monster
    |> Monster.send_scroll("<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price (Experience)</span>       <span class='dark-cyan'>Required Skill</span></p>")
    |> Monster.send_scroll("<p><span class='dark-cyan'>───────────────────────────────────────────────────────────────────────────</span></p>")

    item_template_ids
    |> Enum.map(&ItemTemplate.find/1)
    |> Enum.map(&ItemTemplate.value/1)
    |> Enum.each(fn(%ItemTemplate{name: name, cost: cost} = it) ->
      case ItemTemplate.skill_too_low?(monster, it) do
        {skill_name, requirement} ->
          Monster.send_scroll(monster, "<p><span class='dark-green'>#{String.ljust(name, 30)}</span><span class='dark-cyan'>#{String.ljust(to_string(cost || "Free"), 25)}</span><span class='dark-cyan'>#{requirement} #{skill_name}</span></p>")
        _ ->
          Monster.send_scroll(monster, "<p><span class='dark-green'>#{String.ljust(name, 30)}</span><span class='dark-cyan'>#{cost || "Free"}</span></p>")
      end
    end)
    monster
  end

  def buy(character, room, item) do
    cond do
      !Entity.has_component?(room, Components.Shop) ->
        send_message(character, "scroll", "<p><span class='red'>You cannot BUY if you are not in a shop!</span></p>")
      true ->
        case Systems.Match.one(Components.Shop.items(room), :name_contains, item) do
          nil ->
            send_message(character, "scroll", "<p>\"#{item}\" does not appear to be for sale here.</p>")
          match ->
            spirit = Possession.possessor(character)
            value  = Components.Module.value(match).value
            exp    = Components.Experience.value(spirit)

            if value > exp do
              send_message(character, "scroll", "<p>#{Components.Name.value(match) |> capitalize_first} costs #{value} experience, you only have #{exp}.</p>")
            else
              Components.Experience.add(spirit, -value)
              Systems.Item.spawn_item(match, character)
              send_message(character, "scroll", "<p>You purchase #{Components.Name.value(match)} for #{value} experience.</p>")
            end
        end
    end
  end

  def sell(monster, room, item) do
    cond do
      !Entity.has_component?(room, Components.Shop) ->
        send_message(monster, "scroll", "<p><span class='red'>You cannot SELL if you are not in a shop!</span></p>")
      true ->
        case Systems.Match.one(Components.Items.get_items(monster), :name_contains, item) do
          nil ->
            send_message(monster, "scroll", "<p>You don't have \"#{item}\" to sell!</p>")
          match ->
            spirit = Possession.possessor(monster)

            if spirit do
              exp = trunc(Components.Module.value(match).value / 10)
              Components.Experience.add(spirit, exp)
              Entities.save!(spirit)
              send_message(spirit, "scroll", "<p>You just sold #{Components.Name.value(match)} for #{exp} experience.</p>")
            end
            Components.Items.remove_item(monster, match)
            Entities.save!(monster)
            Entities.delete!(match)
        end
    end
  end
end
