defmodule Systems.HP do
  def max_hp(entity) do
    health_stat   = Components.Health.value(entity)
    hp_roll_total = Enum.reduce(Components.HPRolls.value(entity), 0, fn(roll, total) ->
      roll + total
    end)
    level = Components.Level.value(entity)
    round(((health_stat / 2.0) + hp_roll_total) + (((health_stat - 50) * level) / 16.0))
  end
end
