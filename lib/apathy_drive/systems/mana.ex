defmodule Systems.Mana do
  use Systems.Reload

  def max(seed) when is_integer(seed) do
    trunc(seed * (11 + (seed / 10)))
  end

  def max(entity) do
    intellect = Systems.Stat.modified(entity, "intellect")
    willpower = Systems.Stat.modified(entity, "willpower")

    max trunc((willpower * 2 + intellect) / 3)
  end

end
