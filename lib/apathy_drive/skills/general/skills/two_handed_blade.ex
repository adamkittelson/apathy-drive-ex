defmodule ApathyDrive.Skills.TwoHandedBlade do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Two Handed Blade",
      attributes: ["agility", "strength"],
      traits: %{
        "Two Handed Blade" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Two Handed Blade</span>
      Increases accuracy with large two handed blades such as greatswords and halberds.

      Attribute(s): #{attributes()}
    """
  end
end
