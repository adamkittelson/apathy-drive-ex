defmodule Systems.Effect do
  use Systems.Reload
  import Timer, except: [start: 0]
  use Timex

  def add(entity, effect, duration) do
    key = Time.now(:msecs) * 1000 |> trunc

    Components.Effects.add(entity, key, effect)

    apply_after duration |> seconds do
      Components.Effects.remove(entity, key)
    end
  end
end
