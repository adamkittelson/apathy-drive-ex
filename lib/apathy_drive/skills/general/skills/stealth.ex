defmodule ApathyDrive.Skills.Stealth do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Stealth",
      attributes: ["agility"],
      traits: %{
        "Stealth" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Stealth</span>
      Stealth is the art of moving without being detected.

      Attribute(s): #{attributes()}
    """
  end
end
