defmodule Systems.HP do
  use Systems.Reload

  def max_hp(seed) when is_integer(seed) do
    Float.floor(seed * (11 + (seed / 10)))
  end

  def max_hp(entity) do
    health   = Systems.Stat.modified(entity, "health")
    strength = Systems.Stat.modified(entity, "strength")

    max_hp Float.floor((health * 2 + strength) / 3)
  end
end
