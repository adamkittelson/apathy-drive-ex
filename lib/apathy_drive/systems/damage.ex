defmodule Systems.Damage do
  use Systems.Reload

  @damage_types %{
    aether: "magical",
    cold: "magical",
    crushing: "physical",
    cutting: "physical",
    disruption: "magical",
    electricity: "magical",
    fire: "magical",
    holy: "magical",
    impact: "physical",
    impaling: "physical",
    infernal: "magical",
    plasma: "magical",
    strike: "physical",
    vacuum: "magical",
  }

  def calculate_damage(ability, entity, target) do
    if ability[:damage] do
      limb = Components.Limbs.random_unsevered_limb(target)
      damage_rolls = damages(entity, limb, ability[:damage])
      damage = damage_rolls |> Map.values |> Enum.sum
      {limb, damage}
    else
      {nil, 0}
    end
  end

  def do_damage(target, limb, amount) do
    damage_limb(target, limb, amount)

    if Components.HP.subtract(target, amount) do
      Systems.Prompt.update(target)
      HPRegen.add(target)
    else
      Systems.Death.kill(target)
    end
  end

  def damage_limb(target, limb, total) do
    crippled = Components.Limbs.crippled?(target, limb)
    Components.Limbs.damage_limb(target, limb, total)
    cond do
      Components.Limbs.current_damage(target, limb) >= (Components.Limbs.max_damage(target, limb) * 2) ->
        Systems.Limbs.sever_limb(target, limb)
      !crippled && Components.Limbs.crippled?(target, limb) ->
        Systems.Limbs.cripple_limb(target, limb)
      true ->
    end
  end

  def damages(entity, limb, damages) do
    damages
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage_type, map) ->
        raw = raw_damage(damages[damage_type])
        protection = protection(entity, limb, damage_type)
        damage = Float.ceil(raw * protection) |> trunc
        Map.put(map, damage_type, damage)
       end)
  end

  def damage_reduction(entity, limb, damage_type) do
    1 - protection(entity, limb, damage_type)
  end

  def protection(entity, limb, damage_type) do
    resistance = resistance(entity, @damage_types[damage_type])
    ac = resistance(ac(entity, limb))
    (1 - (resistance_reduction(resistance))) * (1 - (resistance_reduction(ac)))
  end

  def ac(entity, limb) do
    entity
    |> Components.Limbs.items(limb)
    |> Enum.map(&Components.AC.value(&1))
    |> Enum.filter(&is_number(&1))
    |> Enum.sum
  end

  def raw_damage(range) do
    :random.seed(:os.timestamp)
    range |> Enum.into([]) |> Enum.shuffle |> List.first
  end

  def resistance(entity, "physical") do
    physical_resistance(entity)
  end

  def resistance(entity, "magical") do
    magical_resistance(entity)
  end

  def reduced_damage(resistance, damage) do
    trunc(damage * (1 - resistance_reduction(resistance)))
  end

  def physical_resistance(entity) do
    strength = Systems.Stat.modified(entity, "strength")
    agility  = Systems.Stat.modified(entity, "agility")

    resistance(abs((((strength * 3) + agility) / 4) - 40))
  end

  def magical_resistance(entity) do
    willpower = Systems.Stat.modified(entity, "willpower")
    intellect = Systems.Stat.modified(entity, "intellect")

    resistance(abs((((willpower * 3) + intellect) / 4) - 40))
  end

  def resistance(stat) do
    trunc(stat * (0.5 + (stat / 100)))
  end

  def resistance_reduction(resistance) do
    resistance / (250 + resistance)
  end

  def base_damage(seed) when is_integer(seed) do
    seed = seed / 10
    base = trunc(seed * (11 + (seed / 10)))
    Range.new(trunc(base * 0.8), trunc(base * 1.2))
  end

  def base_damage(entity) do
    agility   = Systems.Stat.modified(entity, "agility")
    strength = Systems.Stat.modified(entity, "strength")

    base_damage(trunc((strength * 2 + agility) / 3))
  end

end
