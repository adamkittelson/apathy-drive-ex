defmodule ApathyDrive.Skills.Staff do
  alias ApathyDrive.{Ability, Mobile}
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Staff</span>
      Increases accuracy with staves.

      Attribute(s): #{attributes()}
    """
  end
end
