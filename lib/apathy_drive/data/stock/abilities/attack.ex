defmodule Abilities.Attack do
  use Systems.Ability

  def properties(attacker) do
    attack = get_attack(attacker)

    %{
      target:           "living",
      damage:           attack_damage(attacker, attack),
      user_message:     "<p><span class='red'>#{attack["message"]["attacker"]}</span></p>",
      target_message:   "<p><span class='red'>#{attack["message"]["target"]}</span></p>",
      observer_message: "<p><span class='red'>#{attack["message"]["spectator"]}</span></p>"
    }
  end

  def useable_by?(_entity) do
    false
  end

  def get_attack(entity) do
    Components.Attacks.random(entity)
  end

  def attack_damage(attacker, attack) do
    low..high = Systems.Damage.base_damage(attacker)

    attack["damage"]
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage_type, damage) ->
         Map.put(damage, String.to_atom(damage_type), (trunc(low * attack["damage"][damage_type]))..(trunc(high * attack["damage"][damage_type])))
       end)
  end

  def execute(entity, target) do
    Systems.Ability.execute(properties(entity), entity, target, :execute)
  end

  def help do
    "This spell shoots a shimmering dart of pure mana at the target, causing minor damage."
  end
end
