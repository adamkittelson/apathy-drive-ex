defmodule ApathyDrive.Skills.AmplifyDamage do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "curse",
      duration: duration(level),
      command: "ampd",
      targets: "monster or single",
      energy: 500,
      name: "Amplify Damage",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      auto: character.skills["ampd"] && character.skills["ampd"].auto,
      user_message: "You cast amplify damage on {{target}}!",
      target_message: "{{user}} casts amplify damage on you!",
      spectator_message: "{{user}} casts amplify damage on {{target}}!",
      traits: %{
        "ResistPhysical" => -100,
        "StatusMessage" => "You are vulnerable!"
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Amplify Damage</span>
      This deceptively potent curse rapidly advances the age and putridity of any wound.
    Ordinary blows will cut through flesh and carve particularly vicious wounds that fester and
    seethe.
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Duration: #{duration(level)} seconds
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= Skill.max_level() do
      "\nNext Skill Level: #{level}\nDuration: #{duration(level)} seconds\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 4

  defp duration(level) do
    # 8 - 65
    trunc(3 + level * (level + 4.4))
  end
end
