defmodule ApathyDrive.Skills.Bash do
  alias ApathyDrive.{Character, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    level = skill_level(character)
    weapon = Character.weapon(character)

    character
    |> Mobile.attack_ability()
    |> Map.put(:command, "bash")
    |> Map.put(:name, "bash")
    |> Map.put(:targets, "monster or single")
    |> Map.put(:mana, mana(level))
    |> Map.put(:auto, !!get_in(character, [:skills, "bash", :auto]))
    |> Map.put(:attributes, ["strength"])
    |> Map.put(:cast_time, 2500)
    |> Map.put(:energy, 0)
    |> Map.put(:spell?, false)
    |> Map.put(
      :user_message,
      "You bash {{target}} with your #{weapon.name} for {{amount}} damage!"
    )
    |> Map.put(
      :target_message,
      "{{user}} bashes you with their #{weapon.name} for {{amount}} damage!"
    )
    |> Map.put(
      :spectator_message,
      "{{user}} bashes {{target}} with their #{weapon.name} for {{amount}} damage!"
    )
    |> update_in([Access.key!(:traits)], fn traits ->
      traits
      |> Map.put("Attack%", attack_percent(level))
      |> Map.put("Color", "magenta")
      |> update_in(["Damage"], fn damages ->
        Enum.reduce(damages, [], fn
          %{kind: "physical"} = damage, damages ->
            modifier = (100 + damage_percent(level)) / 100

            damage =
              damage
              |> Map.put(:min, trunc((damage.min + damage(level)) * modifier))
              |> Map.put(:max, trunc((damage.max + damage(level)) * modifier))

            [damage | damages]

          %{} = damage, damages ->
            [damage | damages]
        end)
      end)
    end)
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Bash</span>
      Strike an enemy with great strength, doing increased damage and knocking them back.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Damage: +#{damage(level)}
      Damage Bonus: #{damage_percent(level)}%
      Attack Bonus: #{attack_percent(level)}%
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Damage: +#{damage(level)}\nDamage Bonus: #{damage_percent(level)}%\nAttack Bonus: #{attack_percent(level)}%\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level) do
    2
  end

  defp damage(level) do
    # 1 - 20
    trunc(-2 + level * 3.75)
  end

  defp damage_percent(level) do
    # 50 - 145
    trunc(31 + level * 19)
  end

  defp attack_percent(level) do
    # 20 - 115
    trunc(7 + level * (level + 12))
  end
end
