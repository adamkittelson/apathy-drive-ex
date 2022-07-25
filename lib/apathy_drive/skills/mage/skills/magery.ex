defmodule ApathyDrive.Skills.Magery do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Magery",
      attributes: ["intellect"],
      traits: %{
        "Magery" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Magery</span>
      Increases proficiency at casting magic spells.

      Attribute(s): #{attributes()}
    """
  end
end
