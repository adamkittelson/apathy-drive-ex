defmodule ApathyDrive.Skills.Perception do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Perception",
      attributes: ["intellect"],
      traits: %{
        "Perception" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Perception</span>
      Perception is the ability to detect hidden items, exits, and enemies.

      Attribute(s): #{attributes()}
    """
  end
end
