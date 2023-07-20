defmodule ApathyDrive.Skills.Balance do
  alias ApathyDrive.{Ability, Mobile}
  use ApathyDrive.Skill

  def ability(character) do
    %Ability{
      kind: "passive",
      targets: "self",
      name: "Balance",
      attributes: ["intellect", "willpower"],
      traits: %{
        "Balance" => skill_level(character)
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(_character) do
    """
      <span style="color: lime">Balance</span>
      Increases proficiency with casting spells of nature's healing and protection.

      Attribute(s): #{attributes()}
    """
  end
end
