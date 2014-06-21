defmodule Systems.Mana do
  use Systems.Reload
  def max_mana(entity) do
    intellect = Systems.Stat.modified(entity, "intellect")
    willpower = Systems.Stat.modified(entity, "willpower")
    level  = Components.Level.value(entity)

    Float.floor(4 * (level + 9) + Enum.max([10, level - 25]) * ((intellect + willpower) / 2))
  end
end
