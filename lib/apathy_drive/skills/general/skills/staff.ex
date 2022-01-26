defmodule ApathyDrive.Skills.Staff do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Staff",
      attributes: ["agility", "intellect"],
      traits: %{
        "Staff" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Staff</span>
      Increases accuracy with staves.

      Attribute(s): #{attributes()}
    """
  end
end
