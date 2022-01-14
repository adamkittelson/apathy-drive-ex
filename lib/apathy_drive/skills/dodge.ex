defmodule ApathyDrive.Skills.Dodge do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(_character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Dodge",
      attributes: ["agility"],
      traits: %{}
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Dodge</span>
      Proficiency at dodging enemy attacks.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}"
    end
  end
end
