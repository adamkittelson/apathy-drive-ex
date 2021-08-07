defmodule ApathyDrive.Skills.PolearmMastery do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "mastery",
      targets: "self",
      name: "Polearm Mastery",
      attributes: ["strength"],
      traits: %{
        "Damage%" => damage_percent(level),
        "AttackRating%" => attack_percent(level),
        "Crits" => crit_percent(level)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Polearm Mastery</span>
      Increases damage, attack rating, and critical hit chance while wielding a polearm.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
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

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Damage Bonus: #{damage_percent(level)}%\nAttack Bonus: #{attack_percent(level)}%\nCrit Bonus: #{crit_percent(level)}%"
    end
  end

  defp damage_percent(level) do
    # 28 - 123
    trunc(9 + level * 19)
  end

  defp attack_percent(level) do
    # 28 - 180
    trunc(-2 + level * 30.4)
  end

  defp crit_percent(level) do
    # 5 - 29
    trunc(1 + level * 4.75)
  end
end
