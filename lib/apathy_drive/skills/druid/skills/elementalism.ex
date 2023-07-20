defmodule ApathyDrive.Skills.Elementalism do
  alias ApathyDrive.{Ability, Mobile}
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Elementalism</span>
      Increases proficiency with casting spells of elemental destruction and pestilence.

      Attribute(s): #{attributes()}
    """
  end
end
