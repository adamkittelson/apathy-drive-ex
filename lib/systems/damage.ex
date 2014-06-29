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

  def do_damage(ability, entity, target) do
    if ability.properties[:damage] do
      damages = damages(entity, ability.properties[:damage])
      total = damages |> Map.values |> Enum.sum

      damage_random_limb(target, total)

      if Components.HP.subtract(target, total) do
        Systems.Prompt.update(target)
      else
        Systems.Ability.kill(entity, target)
      end
    end
  end

  def damage_random_limb(target, total) do
    limb = Components.Limbs.random(target)
    crippled = Components.Limbs.crippled?(target, limb)
    Components.Limbs.damage_limb(target, limb, total)
    if !crippled && Components.Limbs.crippled?(target, limb) do
      Components.CurrentRoom.get_current_room(target)
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           cond do
             character == target ->
               Components.Player.send_message(character, ["scroll", "<p>Your #{limb} is crippled!</p>"])
              true ->
               Components.Player.send_message(character, ["scroll", "<p>#{Components.Name.value(target)}'s #{limb} is crippled!</p>"])
           end
         end)
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
