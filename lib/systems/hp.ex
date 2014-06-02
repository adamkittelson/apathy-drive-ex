defmodule Systems.HP do
  use Systems.Reload
  def max_hp(entity) do
    health_stat   = Components.Health.value(entity)
    level = Components.Level.value(entity)
    round(((health_stat / 2.0)) + (((health_stat - 50) * level) / 16.0))
  end
end
