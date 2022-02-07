defmodule ApathyDrive.Skills.Elementalism do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Elementalism",
      attributes: ["intellect", "willpower"],
      traits: %{
        "Elementalism" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Elementalism</span>
      Increases proficiency with casting spells of elemental destruction and pestilence.

      Attribute(s): #{attributes()}
    """
  end
end
