defmodule Systems.Effect do
  use Systems.Reload
  import BlockTimer
  use Timex

  def add(entity, effect) do
    key = Time.now(:msecs) * 1000 |> trunc

    Components.Effects.add(entity, key, effect)
  end

  def add(entity, effect, duration) do
    key = Time.now(:msecs) * 1000 |> trunc

    Components.TimerManager.call_after(entity, {{:effect, key}, duration |> seconds, fn ->
      Components.Effects.remove(entity, key)
    end})

    effect = if effect && effect[:timers] do
      Map.put(effect, :timers, [{:effect, key} | effect[:timers]])
    else
      Map.put(effect, :timers, [{:effect, key}])
    end

    Components.Effects.add(entity, key, effect)
  end

end
