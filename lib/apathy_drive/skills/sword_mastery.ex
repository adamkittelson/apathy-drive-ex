defmodule ApathyDrive.Skills.SwordMastery do
  alias ApathyDrive.{Ability, Mobile}

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "mastery",
      targets: "self",
      name: "Sword Mastery",
      traits: %{
        "Damage%" => damage_percent(level),
        "AttackRating%" => attack_percent(level),
        "Crits" => crit_percent(level)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Sword Mastery</span>
      Increases damage, attack rating, and critical hit chance while wielding a sword.
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Damage Bonus: #{damage_percent(level)}%
      Attack Bonus: #{attack_percent(level)}%
      Crit Bonus: #{crit_percent(level)}%
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= 20 do
      "\nNext Skill Level: #{level}\nDamage Bonus: #{damage_percent(level)}%\nAttack Bonus: #{
        attack_percent(level)
      }%\nCrit Bonus: #{crit_percent(level)}%"
    end
  end

  defp damage_percent(level) do
    23 + level * 5
  end

  defp attack_percent(level) do
    23 + level * 8
  end

  defp crit_percent(level) do
    trunc(4 + level * 1.25)
  end

  defp skill_level(character) do
    character.skills
    |> Map.values()
    |> Enum.find(&(&1.name == "Sword Mastery"))
    |> case do
      %{level: level} ->
        level

      _ ->
        0
    end
  end
end
