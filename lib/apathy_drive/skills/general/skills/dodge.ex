defmodule ApathyDrive.Skills.Dodge do
  alias ApathyDrive.{Ability, Mobile}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Dodge",
      attributes: ["agility"],
      traits: %{
        "Dodge" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Dodge</span>
      Skill at avoding enemy attacks.

      Attribute(s): #{attributes()}
    """
  end
end
