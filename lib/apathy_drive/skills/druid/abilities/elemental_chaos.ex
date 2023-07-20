defmodule ApathyDrive.Skills.ElementalChaos do
  alias ApathyDrive.{Ability, Mobile}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.Elementalism
  @id 30

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    ability = Ability.find(@id)

    ability
    |> Map.put(:mana, mana(ability.mana, level))
    |> Map.put(:spell?, true)
    |> update_in([Access.key!(:traits), "Damage"], &damage(&1, character, level))
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    ability = ability(character)

    """
      <span style="color: lime">#{ability.name}</span>
      #{ability.description}
      Skill: #{@skill.name()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  def casting_skill, do: @skill

  defp current_skill_level(character) do
    level = skill_level(character)
    ability = ability(character)

    damage =
      ability.traits["Damage"]
      |> tooltip_damage()

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      #{damage}
      Mana Cost: #{mana(ability.mana, level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1
    ability = ability(character, level)

    damage =
      ability.traits["Damage"]
      |> tooltip_damage()

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{damage}\nMana Cost: #{mana(ability.mana, level)}"
    end
  end

  defp mana(mana, level), do: trunc(mana / 2 + mana / 2 * level)

  defp damage(damages, character, level) do
    damages
    |> Enum.map(fn damage ->
      damage
      |> Map.put(:min, min_damage(damage[:min], character, level))
      |> Map.put(:max, max_damage(damage[:max], character, level))
    end)
  end

  defp min_damage(damage, character, level) do
    trunc(damage * level * (@skill.skill_level(character) / 100))
  end

  defp max_damage(damage, character, level) do
    trunc(damage * level * (@skill.skill_level(character) / 100))
  end

  defp tooltip_damage(damages) do
    Enum.map(damages, fn damage ->
      "#{damage.damage_type} Damage: #{damage.min}-#{damage.max}"
    end)
    |> Enum.join("\n")
  end
end
