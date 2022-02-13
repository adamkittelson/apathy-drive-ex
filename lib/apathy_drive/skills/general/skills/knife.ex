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
        "Knife" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Knife</span>
      Increases accuracy with small bladed weapons such as knives and daggers.

      Attribute(s): #{attributes()}
    """
  end
end
