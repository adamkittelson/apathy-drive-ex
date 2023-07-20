defmodule ApathyDrive.Skills.Melee do
  alias ApathyDrive.{Ability, Mobile}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Melee",
      attributes: ["agility", "strength"],
      traits: %{
        "Melee" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Melee</span>

      Increases accuracy when fighting bare-handed.

      Attribute(s): #{attributes()}
    """
  end
end
