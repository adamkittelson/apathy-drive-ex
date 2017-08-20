defmodule ApathyDrive.Level do

  def exp_to_next_level(current_level, current_exp) do
    exp_at_level(current_level + 1) - current_exp
  end

  def exp_to_next_skill_level(current_level, current_exp, multiplier) do
    trunc(exp_to_next_level(current_level + 1, current_exp) * multiplier)
  end

  def exp_for_level(1, _multiplier), do: 0
  def exp_for_level(lvl, multiplier) do
    ((1 + :math.pow(lvl - 2, 1.50 + (lvl / 75.0))) * 100000 / 150) * multiplier
    |> trunc
  end

  def exp_at_level(exp, multiplier \\ 1.0)
  def exp_at_level(0, _multiplier), do: nil
  def exp_at_level(level, multiplier) do
    (1..level)
    |> Enum.reduce(0, fn(lvl, total) ->
         total + exp_for_level(lvl, multiplier)
       end)
  end

  def exp_reward(level) do
    needed = exp_at_level(level + 1) - exp_at_level(level)

    div(needed, 10 * level)
  end

  def level_at_exp(exp, multiplier \\ 1.0, level \\ 1) do
    if exp_at_level(level, multiplier) > exp do
      max(level - 1, 1)
    else
      level_at_exp(exp, multiplier, level + 1)
    end
  end

  def skill_level_at_exp(exp, multiplier) do
    level_at_exp(exp, multiplier) - 1
  end

  def advance(entity) do
    advance(entity, level_at_exp(entity.experience))
  end

  def advance(%{} = entity, level) do
    put_in(entity.level, level)
  end

  def display_exp_table do
    Enum.each(1..50, fn(level) ->
      essence = String.pad_leading("#{exp_at_level(level)}", 8)
      essence_reward = String.pad_leading("#{exp_reward(level)}", 6)
      level = String.pad_leading("#{level}", 2)

      IO.puts "Level: #{level}, Essence Required: #{essence}, Essence per kill: #{essence_reward}"
    end)
  end

end
