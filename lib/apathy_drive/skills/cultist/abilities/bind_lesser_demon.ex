defmodule ApathyDrive.Skills.BindLesserDemon do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def prereq(), do: ApathyDrive.Skills.SummonLesserDemon

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "attack",
      command: "lbin",
      targets: "self",
      energy: 0,
      name: "bind lesser demon",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "lbin", :auto]),
      traits: %{
        "Script" => "bind_lesser_demon",
        "ControlChance" => control_chance(level),
        "Duration" => duration(level),
        "Replenishment" => replenishment(level),
        "Defense" => defense(level)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Bind Lesser Demon</span>

      This spell binds a lesser demon, granting you some of its power.

      Attribute(s): #{attributes()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Defense: #{defense(level)}
      Replenishment: #{replenishment(level)}
      Success Chance: #{control_chance(level)}%
      Duration: #{div(duration(level), 60)} minutes
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Defense: #{defense(level)}\nSuccess Chance: #{control_chance(level)}%\nDuration: #{div(duration(level), 60)} minutes\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 6

  defp control_chance(level) do
    15 * level
  end

  defp duration(level) do
    600 * level
  end

  defp defense(level) do
    6 * level
  end

  defp replenishment(level) do
    level
  end
end
