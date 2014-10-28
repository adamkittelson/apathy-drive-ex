defmodule Systems.Cooldown do

  def on_cooldown?(entity, ability_name) do
    entity
    |> Components.Effects.value
    |> Map.values
    |> Enum.any?(fn(effect) ->
         effect[:cooldown] == ability_name
       end)
  end

  def cooldown_remaining(entity, ability_name) do
    effect = entity
             |> Components.Effects.value
             |> Map.values
             |> Enum.find(fn(effect) ->
                  effect[:cooldown] == ability_name
                end)
    case effect do
      nil -> 0
      effect -> effect[:cooldown_remaining]
    end
  end
end
