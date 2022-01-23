defmodule ApathyDrive.Skills.SummonLesserDemon do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "attack",
      command: "ldem",
      targets: "self",
      energy: 0,
      name: "summon lesser demon",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "ldem", :auto]),
      traits: %{
        "Script" => "summon_lesser_demon",
        "ControlChance" => control_chance(level),
        "Duration" => duration(level)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Summon Lesser Demon</span>

      Summons a lesser demon to fight by your side.

      Attribute(s): #{attributes()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Control Chance: #{control_chance(level)}%
      Duration: #{div(duration(level), 60)} minutes
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Control Chance: #{control_chance(level)}%\nDuration: #{div(duration(level), 60)} minutes\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 6

  defp control_chance(level) do
    15 * level
  end

  defp duration(level) do
    600 * level
  end
end
