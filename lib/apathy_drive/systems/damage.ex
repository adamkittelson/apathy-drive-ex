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
    if ability.properties[:damage] do
      damages = damages(entity, ability.properties[:damage])
      damages |> Map.values |> Enum.sum
    end
  end

  def do_damage(target, amount) do
    damage_random_limb(target, amount)

    if Components.HP.subtract(target, amount) do
      Systems.Prompt.update(target)
      HPRegen.add(target)
    end
  end

  def damage_random_limb(target, total) do
    limb = Components.Limbs.random_unsevered_limb(target)
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

  def damages(entity, damages) do
    damages
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage, map) ->
        raw = raw_damage(damages[damage])
        Map.put(map, damage, reduced_damage(entity, @damage_types[damage], raw))
       end)
  end

  def raw_damage(range) do
    :random.seed(:os.timestamp)
    range |> Enum.into([]) |> Enum.shuffle |> List.first
  end

  def reduced_damage(entity, "physical", amount) do
    entity
    |> physical_resistance
    |> reduced_damage(amount)
  end

  def reduced_damage(entity, "magical", amount) do
    entity
    |> magical_resistance
    |> reduced_damage(amount)
  end

  def reduced_damage(resistance, damage) do
    Float.floor(damage * (1 - resistance_reduction(resistance)))
  end

  def physical_resistance(entity) do
    strength = Systems.Stat.modified(entity, "strength")
    agility  = Systems.Stat.modified(entity, "agility")

    resistance(((strength * 3) + agility) / 4)
  end

  def magical_resistance(entity) do
    willpower = Systems.Stat.modified(entity, "willpower")
    intellect = Systems.Stat.modified(entity, "intellect")

    resistance(((willpower * 3) + intellect) / 4)
  end

  def resistance(stat) do
    Float.floor(stat * (0.5 + (stat / 100)))
  end

  def resistance_reduction(resistance) do
    resistance / (250 + resistance)
  end

end
