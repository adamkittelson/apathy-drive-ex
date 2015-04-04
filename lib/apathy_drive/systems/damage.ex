defmodule Systems.Damage do

  def calculate_damage(ability, target) do
    limb = Components.Limbs.random_unsevered_limb(target)

    case ability[:damage] do
      %{} = damage ->
        damage_rolls = damages(target, limb, damage)
        damage = damage_rolls |> Map.values |> Enum.sum
        crit = get_crit(Map.keys(damage_rolls), damage, target)
        if crit && crit[:damage] do
          damage = damage + (damage * crit[:damage])
        end
        {limb, max(damage, 1), crit}
      damage when is_number damage ->
        {limb, max(damage, 1), nil}
      _ ->
        {nil, 0, nil}
    end
  end

  def get_crit(damage_types, damage, target) do
    :random.seed(:os.timestamp)
    damage_type = damage_types |> Enum.shuffle |> List.first
    chance = trunc((damage / Components.HP.value(target)) * 100)

    if CritTables.find(damage_type) do
      CritTables.find(damage_type).random(chance)
    else
      nil
    end
  end

  def do_damage(target, amount) do
    if Process.alive?(target) do
      if Components.HP.subtract(target, amount) do
        target
        |> Possession.possessor
        |> Systems.Prompt.update(target)
        Systems.Regen.initialize_regen(target)
      else
        Systems.Death.kill(target)
      end
    end
  end

  def damage_limb(target, limb, total) do
    if Process.alive?(target) do
      crippled = Components.Limbs.crippled?(target, limb)
      Components.Limbs.damage_limb(target, limb, total)
      cond do
        Components.Limbs.current_damage(target, limb) >= (Components.Limbs.max_damage(target, limb) * 2) ->
          Systems.Limbs.sever_limb(target, limb)
          if Components.Limbs.fatal_if_severed?(target, limb) do
            :fatal
          end
        !crippled && Components.Limbs.crippled?(target, limb) ->
          Systems.Limbs.cripple_limb(target, limb)
        true ->
      end
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
    resistance = resistance(entity, CritTables.damage_types[to_string(damage_type)])
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

  def resistance(_entity, nil) do
    0
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

    resistance(abs((((strength * 3) + agility) / 4)))
  end

  def magical_resistance(entity) do
    willpower = Systems.Stat.modified(entity, "willpower")
    intellect = Systems.Stat.modified(entity, "intellect")

    resistance(abs((((willpower * 3) + intellect) / 4)))
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

  def base_attack_damage(entity) do
    agility   = Systems.Stat.modified(entity, "agility")
    strength = Systems.Stat.modified(entity, "strength")

    base_damage(trunc((strength * 2 + agility) / 3))
  end

  def base_magic_damage(entity) do
    willpower = Systems.Stat.modified(entity, "willpower")
    intellect = Systems.Stat.modified(entity, "intellect")

    base_damage(trunc((intellect * 2 + willpower) / 3))
  end

end
