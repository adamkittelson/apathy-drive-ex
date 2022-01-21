defmodule ApathyDrive.Skills.Dodge do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Dodge",
      attributes: ["agility"],
      traits: %{
        "dodge" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Dodge</span>
      Skill at avoding enemy attacks.

      Attribute(s): #{attributes()}
    """
  end
end
