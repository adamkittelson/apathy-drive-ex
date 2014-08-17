defmodule Systems.Effect do
  use Systems.Reload
  import BlockTimer
  use Timex

  def add(entity, effect, duration) do
    key = Time.now(:msecs) * 1000 |> trunc

    {:ok, timer} = apply_after duration |> seconds do
      Components.Effects.remove(entity, key)
    end

    effect = if effect && effect[:timers] do
      Map.put(effect, :timers, [timer | effect[:timers]])
    else
      Map.put(effect, :timers, [timer])
    end

    Components.Effects.add(entity, key, effect)
  end
end
