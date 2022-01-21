defmodule ApathyDrive.Skills.TwoHandedBlunt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Two Handed Blunt",
      attributes: ["strength"],
      traits: %{
        "two handed blunt" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Two Handed Blade</span>
      Increases accuracy with large two handed blunt weapons such as mauls and flails.

      Attribute(s): #{attributes()}
    """
  end
end
