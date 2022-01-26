defmodule ApathyDrive.Skills.NatureMagic do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Nature Magic",
      attributes: ["intellect", "willpower"],
      traits: %{
        "Nature Magic" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Nature Magic</span>
      Increases proficiency with casting spells related to nature.

      Attribute(s): #{attributes()}
    """
  end
end
