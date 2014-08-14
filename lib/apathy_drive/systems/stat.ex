defmodule Systems.Stat do
  use Systems.Reload

  def base(entity) do
    Components.Stats.value(entity)
  end

  def base(entity, stat) do
    Components.Stats.value(entity)[stat]
  end

  def modified(entity) do
    base(entity)
    |> Map.keys
    |> Enum.reduce(%{}, fn(stat, stats) ->
         Map.put(stats, stat, modified(entity, stat))
       end)
  end

  def pre_effects_bonus(entity) do
    base(entity)
    |> Map.keys
    |> Enum.reduce(%{}, fn(stat, stats) ->
         Map.put(stats, stat, pre_effects_bonus(entity, stat))
       end)
  end

  def pre_effects_bonus(entity, stat) do
    base(entity, stat) + skill_bonus(entity, stat)
  end

  def modified(entity, stat) do
    pre_effects_bonus(entity, stat) + effects_bonus(entity, stat)
  end

  def skill_bonus(entity, stat) do
    entity
    |> Components.Skills.list
    |> Enum.map(&(Skills.find(&1)))
    |> Enum.filter(fn(skill) ->
         skill.modifiers
         |> Map.keys
         |> Enum.member?(:"#{stat}")
       end)
    |> Enum.reduce(0, fn(skill, total_stat_modification) ->
         base = skill.trained(entity)
         percentage = skill.modifiers[:"#{stat}"] / (skill.modifiers
                                                     |> Map.values
                                                     |> Enum.sum)
         total_stat_modification + base * percentage
       end)
    |> trunc
  end

  def effects_bonus(entity, stat) do
    entity
    |> Components.Effects.value
    |> Map.values
    |> Enum.map(&(&1[stat] || 0))
    |> Enum.sum
  end

end
