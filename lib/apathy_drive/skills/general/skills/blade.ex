defmodule ApathyDrive.Skills.Blade do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Blade",
      attributes: ["agility", "strength"],
      traits: %{
        "Blade" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Blade</span>
      Increases accuracy with large one handed blades such as swords and axes.

      Attribute(s): #{attributes()}
    """
  end
end
