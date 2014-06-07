defmodule Systems.Stat do
  use Systems.Reload

  def base(entity, stat) do
    Components.Stats.value(entity)[stat]
  end

  def modified(entity, stat) do
    base(entity, stat) + bonus(entity, stat)
  end

  def bonus(entity, stat) do
    entity
    |> Components.Skills.value
    |> Map.keys
    |> Enum.map(&(Skills.find(&1)))
    |> Enum.filter(fn(skill) ->
         skill.modifiers
         |> Map.keys
         |> Enum.member?(:"#{stat}")
       end)
    |> Enum.reduce(0, fn(skill, total_stat_modification) ->
         base = skill.base(entity)
         percentage = skill.modifiers[:"#{stat}"] / (skill.modifiers
                                                     |> Map.values
                                                     |> Enum.sum)
         total_stat_modification + base * percentage
       end)
    |> Float.floor
  end

end
