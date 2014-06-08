defmodule Systems.HP do
  use Systems.Reload
  def max_hp(entity) do
    health = Systems.Stat.modified(entity, "health")
    level  = Components.Level.value(entity)

    4 * (level + 9) + Enum.max([10, level - 25]) * health
  end
end
