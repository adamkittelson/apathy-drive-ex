defmodule Systems.Description do
  use Systems.Reload
  import Utility
  import Systems.Text

  def add_description_to_scroll(%Spirit{} = spirit, monster) when is_pid(monster) do
    add_description_to_scroll(spirit, Monster.value(monster))
  end
  def add_description_to_scroll(%Spirit{} = spirit, %Monster{} = monster) do
    spirit
    |> Spirit.send_scroll("<p><span class='cyan'>#{monster.name}</span></p>")
    |> Spirit.send_scroll("<p>#{monster.description}</p>")
    |> Spirit.send_scroll("<p>#{describe_hp(monster) |> interpolate(%{"target" => monster})}</p>")
    # if Entity.has_component?(target, Components.Limbs) do
    #   limbs = Components.Limbs.value(target)
    #   equipped_items = Systems.Limbs.equipped_items(target)
    #
    #   if equipped_items |> Enum.count > 0 do
    #     msg = "<p>\n<span class='dark-yellow'>{{target:He/She/It}} is equipped with:</span></p>" |> interpolate(%{"target" => target})
    #     send_message(character, "scroll", "#{msg}<br>")
    #     equipped_items |> Enum.each fn(item) ->
    #       item_name = Components.Name.value(item)
    #       item_limbs = Systems.Limbs.get_limb_names(limbs, item)
    #       send_message(character, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 23)}</span><span class='dark-cyan'>(#{Enum.join(item_limbs, ", ")})</span></p>")
    #     end
    #   end
    # end
  end

  def add_description_to_scroll(%Monster{} = monster, target) when is_pid(target) do
    add_description_to_scroll(monster, Monster.value(target))
  end
  def add_description_to_scroll(%Monster{} = monster, %Monster{} = target) do
    monster
    |> Monster.send_scroll("<p><span class='cyan'>#{target.name}</span></p>")
    |> Monster.send_scroll("<p>#{target.description}</p>")
    |> Monster.send_scroll("<p>#{describe_hp(target) |> interpolate(%{"target" => target})}</p>")
    # if Entity.has_component?(target, Components.Limbs) do
    #   limbs = Components.Limbs.value(target)
    #   equipped_items = Systems.Limbs.equipped_items(target)
    #
    #   if equipped_items |> Enum.count > 0 do
    #     msg = "<p>\n<span class='dark-yellow'>{{target:He/She/It}} is equipped with:</span></p>" |> interpolate(%{"target" => target})
    #     send_message(character, "scroll", "#{msg}<br>")
    #     equipped_items |> Enum.each fn(item) ->
    #       item_name = Components.Name.value(item)
    #       item_limbs = Systems.Limbs.get_limb_names(limbs, item)
    #       send_message(character, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 23)}</span><span class='dark-cyan'>(#{Enum.join(item_limbs, ", ")})</span></p>")
    #     end
    #   end
    # end
  end

  def describe_hp(%Monster{} = monster) do
    percentage = round(100 * (monster.hp / Monster.max_hp(monster)))
    description = case percentage do
      _ when percentage >= 100 ->
        "unwounded"
      _ when percentage >= 90 ->
        "slightly wounded"
      _ when percentage >= 60 ->
        "moderately wounded"
      _ when percentage >= 40 ->
        "heavily wounded"
      _ when percentage >= 20 ->
        "severely wounded"
      _ when percentage >= 10 ->
        "critically wounded"
      _ ->
        "very critically wounded"
    end
    "{{target:He/She/It}} appears to be #{description}."
  end

end
