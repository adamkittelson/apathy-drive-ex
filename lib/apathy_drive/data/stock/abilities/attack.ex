defmodule Abilities.Attack do
  use Systems.Ability

  def properties(attacker) do
    weapon = get_attack(attacker)

    %{
      target:           "living",
      damage:           weapon_damage(attacker, weapon),
      user_message:     "<p><span class='red'>#{weapon["message"]["attacker"]}</span></p>",
      target_message:   "<p><span class='red'>#{weapon["message"]["target"]}</span></p>",
      observer_message: "<p><span class='red'>#{weapon["message"]["spectator"]}</span></p>"
    }
  end

  def useable_by?(entity) do
    false
  end

  def get_attack(caster) do
    :random.seed(:os.timestamp)
    race = Components.Race.value(caster)
    |> Components.Module.value

    race.attacks
    |> Enum.shuffle
    |> List.first
  end

  def weapon_damage(attacker, weapon) do
    low..high = Systems.Damage.base_damage(attacker)

    weapon["damage"]
    |> Map.keys
    |> Enum.reduce(%{}, fn(damage_type, damage) ->
         Map.put(damage, String.to_atom(damage_type), (trunc(low * weapon["damage"][damage_type]))..(trunc(high * weapon["damage"][damage_type])))
       end)
  end

  def execute(entity, target) do
    Systems.Ability.execute(__MODULE__, entity, target, :execute)
  end

  def help do
    "This spell shoots a shimmering dart of pure mana at the target, causing minor damage."
  end
end
