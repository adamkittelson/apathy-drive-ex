defmodule ApathyDrive.Skills.Blade do
  alias ApathyDrive.{Ability, Mobile}
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Blade</span>
      Increases accuracy with large one handed blades such as swords and axes.

      Attribute(s): #{attributes()}
    """
  end
end
