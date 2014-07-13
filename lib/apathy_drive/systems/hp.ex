defmodule Systems.HP do
  use Systems.Reload

  def max(seed) when is_integer(seed) do
    trunc(seed * (11 + (seed / 10)))
  end

  def max(entity) do
    health   = Systems.Stat.modified(entity, "health")
    strength = Systems.Stat.modified(entity, "strength")

    max trunc((health * 2 + strength) / 3)
  end
end
