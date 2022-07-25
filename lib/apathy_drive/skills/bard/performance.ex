defmodule ApathyDrive.Skills.Performance do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Performance",
      attributes: ["charm"],
      traits: %{
        "Performance" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Performance</span>
      Increases proficiency at performing songs with magical effects upon those who hear them.

      Attribute(s): #{attributes()}
    """
  end
end
