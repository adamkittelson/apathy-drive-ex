defmodule ApathyDrive.Skills.Bash do
  alias ApathyDrive.{Character, Mobile}

  def ability(character) do
    level = skill_level(character)
    weapon = Character.weapon(character)

    character
    |> Mobile.attack_ability()
    |> Map.put(:command, "bash")
    |> Map.put(:name, "bash")
    |> Map.put(:targets, "monster or single")
    |> Map.put(:mana, mana(level))
    |> Map.put(:auto, character.skills["bash"].auto)
    |> Map.put(:attributes, ["strength"])
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Bash</span>
      Strike an enemy with great strength, doing increased damage and knocking them back.
      #{current_skill_level(character)}#{next_skill_level(character)}
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

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= 20 do
      "\nNext Skill Level: #{level}\nDamage: +#{damage(level)}\nDamage Bonus: #{
        damage_percent(level)
      }%\nAttack Bonus: #{attack_percent(level)}%\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level) do
    2
  end

  defp damage(level) do
    level
  end

  defp damage_percent(level) do
    45 + level * 5
  end

  defp attack_percent(level) do
    15 + level * 5
  end

  defp skill_level(character) do
    character.skills
    |> Map.values()
    |> Enum.find(&(&1.name == "bash"))
    |> case do
      %{level: level} ->
        level

      _ ->
        0
    end
  end
end
