defmodule ApathyDrive.Skills.ChargedBolt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "cbol",
      targets: "monster or single",
      energy: 500,
      name: "charged bolt",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      auto: character.skills["cbol"] && character.skills["cbol"].auto,
      user_message: "You fire a charged bolt at {{target}} for {{amount}} damage!",
      target_message: "{{user}} fire a charged bolt at you for {{amount}} damage!",
      spectator_message: "{{user}} fires a charged bolt at {{target}} for {{amount}} damage!",
      traits: %{
        "Damage" => [
          %{
            damage_type: "Electricity",
            min: min_damage(level),
            max: max_damage(level)
          }
        ]
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Charged Bolt</span>
      This spell shoots a bolt of pure electricity at the target, causing minor damage.
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Electricity Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= Skill.max_level() do
      "\nNext Skill Level: #{level}\nElectricity Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 2.5

  defp min_damage(level) do
    trunc(1 + level * 2.2)
  end

  defp max_damage(level) do
    trunc(4 + level * 2.8)
  end
end
