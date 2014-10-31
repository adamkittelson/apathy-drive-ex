defmodule Abilities.Attack do
  use Systems.Ability

  def properties(attacker) do
    attack = get_attack(attacker)

    delay = attacker
            |> Systems.Stat.modified("agility")
            |> get_delay(attack)

    props = %{
      damage:            attack_damage(attacker, attack),
      delay:             delay,
      skill:             attack["skill"],
      user_message:      "<p><span class='red'>#{attack["message"]["attacker"]}</span></p>",
      target_message:    "<p><span class='red'>#{attack["message"]["target"]}</span></p>",
      spectator_message: "<p><span class='red'>#{attack["message"]["spectator"]}</span></p>"
    }

    Map.merge(super(attacker), props)
  end

  def dodgeable, do: true
  def parryable, do: true

  def get_delay(agility, attack) do
    base = attack["speed"] || 3.0

    base = (base * 2) - (agility / 50)
    Enum.max([base, 0.1])
  end

  def get_attack(entity) do
    Components.Attacks.random(entity)
  end

  def attack_damage(attacker, attack) do
    low..high = Systems.Damage.base_attack_damage(attacker)

    attack["damage"]
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage_type, damage) ->
         Map.put(damage, damage_type, (trunc(low * attack["damage"][damage_type]))..(trunc(high * attack["damage"][damage_type])))
       end)
  end

  def execute(entity, target) do
    attack = properties(entity)
    Systems.Ability.execute(attack, entity, target, :dodge)
    attack[:delay]
  end

  def help do
    "This spell shoots a shimmering dart of pure mana at the target, causing minor damage."
  end
end
