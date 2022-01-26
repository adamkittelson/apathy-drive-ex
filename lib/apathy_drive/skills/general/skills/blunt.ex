defmodule ApathyDrive.Skills.Blunt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Blunt",
      attributes: ["agility", "strength", "willpower"],
      traits: %{
        "Blunt" => skill_level(character)
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(_character, _skill) do
    """
      <span style="color: lime">Blunt</span>
      Increases accuracy with one-handed blunt weapons such as maces and clubs.

      Attribute(s): #{attributes()}
    """
  end
end
