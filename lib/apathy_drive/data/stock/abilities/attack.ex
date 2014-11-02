defmodule Abilities.Attack do
  use Systems.Ability

  def properties(attacker) do
    default = super(attacker)
    properties(default, attacker, Systems.Limbs.equipped_weapons(attacker))
  end

  def properties(default, attacker, []) do
    :random.seed(:os.timestamp)

    verb = Components.Module.value(attacker).hit_verbs
           |> Enum.shuffle
           |> List.first

    props = %{
      damage:            attack_damage(attacker, Components.Module.value(attacker).damage, attacker),
      delay:             delay(Systems.Stat.modified(attacker, "agility"), 3.0),
      skill:             "melee",
      user_message:      "<p><span class='red'>You #{verb} {{target}} for {{damage}} damage!</span></p>",
      target_message:    "<p><span class='red'>{{User}} #{Inflex.pluralize(verb)} you for {{damage}} damage!</span></p>",
      spectator_message: "<p><span class='red'>{{User}} #{Inflex.pluralize(verb)} {{target}} for {{damage}} damage!</span></p>"
    }

    Map.merge(default, props)
  end

  def properties(default, attacker, weapons) do
    :random.seed(:os.timestamp)

    weapon = weapons
             |> Enum.shuffle
             |> List.first

    verb = Components.Module.value(weapon).hit_verbs
           |> Enum.shuffle
           |> List.first

    props = %{
      damage:            attack_damage(attacker, Components.Module.value(weapon).damage, weapon),
      delay:             delay(Systems.Stat.modified(attacker, "agility"), Components.Module.value(weapon).speed),
      skill:             Components.Module.value(weapon).accuracy_skill,
      user_message:      "<p><span class='red'>You #{verb} {{target}} with your #{Components.Name.value(weapon)} for {{damage}} damage!</span></p>",
      target_message:    "<p><span class='red'>{{User}} #{Inflex.pluralize(verb)} you with their #{Components.Name.value(weapon)} for {{damage}} damage!</span></p>",
      spectator_message: "<p><span class='red'>{{User}} #{Inflex.pluralize(verb)} {{target}} with their #{Components.Name.value(weapon)} for {{damage}} damage!</span></p>"
    }

    Map.merge(default, props)
  end

  def dodgeable, do: true
  def parryable, do: true

  def delay(agility, speed) do
    speed = (speed * 2) - (agility / 50)
    Enum.max([speed, 0.1])
  end

  def attack_damage(attacker, damage_multipliers, weapon) do
    damage_increases = Components.Effects.damage_increases(weapon)

    damage_multipliers = damage_increases
                         |> Enum.reduce(damage_multipliers, fn(damage_increase, attack) ->
                              damage_increase
                              |> Map.keys
                              |> Enum.reduce(damage_multipliers, fn(table, damage_multipliers) ->
                                   update_in(damage_multipliers, [table], &((&1 || 0) + damage_increase[table]))
                                 end)
                            end)

    low..high = Systems.Damage.base_attack_damage(attacker)

    damage_multipliers
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage_type, damage) ->
         Map.put(damage, damage_type, (trunc(low * damage_multipliers[damage_type]))..(trunc(high * damage_multipliers[damage_type])))
       end)
  end

  def execute(entity, target) do
    attack = properties(entity)
    Systems.Ability.execute(attack, entity, target, :dodge)
    attack[:delay]
  end

  def help do
    "Begin combat with another monster."
  end
end
