defmodule Systems.Mana do
  use Systems.Reload

  def max(seed) when is_integer(seed) do
    x = trunc(seed * (0.5 + (seed / 100)))
    y = x / (125 + x)
    trunc((x / 10) + (x * (1 - y)))
  end

  def max(entity) do
    intellect = Systems.Stat.modified(entity, "intellect")
    willpower = Systems.Stat.modified(entity, "willpower")

    max trunc((willpower * 2 + intellect) / 3)
  end

end
