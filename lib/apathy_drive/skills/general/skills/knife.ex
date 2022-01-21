defmodule ApathyDrive.Skills.Knife do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Knife",
      attributes: ["agility", "intellect"],
      traits: %{
        "knife" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Knife</span>
      Increases accuracy with small bladed weapons such as knives and daggers.

      Attribute(s): #{attributes()}
    """
  end
end
