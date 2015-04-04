defmodule Systems.Description do
  import Systems.Text

  def add_description_to_scroll(%Spirit{} = spirit, monster) when is_pid(monster) do
    add_description_to_scroll(spirit, Monster.value(monster))
  end
  def add_description_to_scroll(%Spirit{} = spirit, %Monster{} = monster) do
    if description = Monster.effect_description(monster) do
      Spirit.send_scroll(spirit, "<p>#{description}</p>")
    else
      spirit
      |> Spirit.send_scroll("<p><span class='cyan'>#{monster.name}</span></p>")
      |> Spirit.send_scroll("<p>#{monster.description}</p>")
      |> Spirit.send_scroll("<p>#{describe_hp(monster) |> interpolate(%{"target" => monster})}</p>")
    end
  end

  def add_description_to_scroll(%Monster{} = monster, target) when is_pid(target) do
    add_description_to_scroll(monster, Monster.value(target))
  end
  def add_description_to_scroll(%Monster{} = monster, %Monster{} = target) do
    if description = Monster.effect_description(target) do
      Monster.send_scroll(monster, "<p>#{description}</p>")
    else
      monster
      |> Monster.send_scroll("<p><span class='cyan'>#{target.name}</span></p>")
      |> Monster.send_scroll("<p>#{target.description}</p>")
      |> Monster.send_scroll("<p>#{describe_hp(target) |> interpolate(%{"target" => target})}</p>")
    end
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
