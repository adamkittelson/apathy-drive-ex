defmodule ApathyDrive.Skills.VineStrike do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.Elementalism
  @id 25

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    ability = Ability.find(@id)

    ability
    |> Map.put(:mana, mana(ability.mana, level))
    |> Map.put(:spell?, true)
    |> update_in([Access.key!(:traits), "Damage"], &damage(&1, character, level))

    # %Ability{
    #   kind: "attack",
    #   command: "vine",
    #   targets: "monster or single",
    #   name: "vine strike",
    #   attributes: @skill.ability(character).attributes,
    #   mana: mana(level),
    #   spell?: true,
    #   energy: 0,
    #   cast_time: 2500,
    #   auto: !!get_in(character, [:skills, "vine", :auto]),
    #   user_message: "You cast vine strike at {{target}} for {{amount}} damage!",
    #   target_message: "{{user}} casts vine strike at you for {{amount}} damage!",
    #   spectator_message: "{{user}} casts vine strike at {{target}} for {{amount}} damage!",
    #   traits: %{
    #     "Damage" => [
    #       %{
    #         damage_type: "Cutting",
    #         min: min_damage(character, level),
    #         max: max_damage(character, level)
    #       }
    #     ]
    #   }
    # }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    ability = ability(character)

    """
      <span style="color: lime">#{ability.name}</span>
      #{ability.description}
      Skill: #{@skill.name()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)
    ability = ability(character)

    damage =
      ability.traits["Damage"]
      |> damage(character, level)
      |> tooltip_damage()

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      #{damage}
      Mana Cost: #{mana(ability.mana, level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1
    ability = ability(character)

    damage =
      ability.traits["Damage"]
      |> damage(character, level)
      |> tooltip_damage()

    if level <= skill.max_level do
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
    trunc(damage * level * (0.5 + @skill.skill_level(character) / 100))
  end

  defp max_damage(damage, character, level) do
    # 9-12
    trunc(damage * level * (0.5 + @skill.skill_level(character) / 100))
  end

  defp tooltip_damage(damages) do
    Enum.map(damages, fn damage ->
      "#{damage.damage_type} Damage: #{damage.min}-#{damage.max}"
    end)
    |> Enum.join("\n")
  end
end
