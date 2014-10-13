defmodule Systems.Level do
  import Systems.Text
  use Systems.Reload
  import Utility

  def exp_to_next_level(entity) do
    exp_at_level(Components.Level.value(entity) + 1) - Components.Experience.value(entity)
  end

  def exp_for_level(1), do: 0
  def exp_for_level(lvl) do
    ((1 + :math.pow(lvl - 2, 1.50 + (lvl / 75.0))) * 100000 / 150)
    |> trunc
  end

  def exp_at_level(0), do: nil
  def exp_at_level(level) do
    (1..level)
    |> Enum.reduce(0, fn(lvl, total) ->
         total + exp_for_level(lvl)
       end)
  end

  def advance(entity) do
    advance(entity, exp_to_next_level(entity))
  end

  def advance(entity, exp_tnl) when exp_tnl < 1 do
    power = Systems.Trainer.total_power(entity)
    Components.Level.advance(entity)
    new_power = Systems.Trainer.total_power(entity)
    if Possession.possessor(entity) do
      message = "#{Components.Name.value(entity)}'s level has increased to #{Components.Level.value(entity)}!"
      send_message(entity, "scroll", "<p>#{capitalize_first(message)}<p>")
      message = "#{Components.Name.value(entity)}'s power has increased by #{new_power - power}."
      send_message(entity, "scroll", "<p>#{capitalize_first(message)}<p>")
    else
      message = "Your level has increased to #{Components.Level.value(entity)}!"
      send_message(entity, "scroll", "<p>#{message}<p>")
      message = "Your power has increased by #{new_power - power}."
      send_message(entity, "scroll", "<p>#{capitalize_first(message)}<p>")
    end
  end
  def advance(_, _), do: nil

end
