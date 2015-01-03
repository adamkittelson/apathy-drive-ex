defmodule Systems.Cooldown do
  use Systems.Reload

  def on_cooldown?(entity, ability_name) do
    entity
    |> Components.TimerManager.value
    |> TimerManager.timers
    |> Enum.member? {:cooldown, ability_name}
  end

  def cooldown_remaining(entity, ability_name) do
    remaining = entity
                |> Components.TimerManager.value
                |> TimerManager.time_remaining({:cooldown, ability_name})

    Float.ceil(remaining / 1000)
    |> trunc
  end
end
