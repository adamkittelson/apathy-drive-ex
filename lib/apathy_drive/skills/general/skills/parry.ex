defmodule ApathyDrive.Skills.Parry do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Parry",
      attributes: ["agility"],
      traits: %{
        "Parry" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Parry</span>
      Parrying lets you deflect enemy attacks with your weapon.

      Attribute(s): #{attributes()}
    """
  end
end
