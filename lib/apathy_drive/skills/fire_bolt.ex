defmodule ApathyDrive.Skills.FireBolt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "fbol",
      targets: "monster or single",
      energy: 500,
      name: "ice bolt",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      auto: character.skills["fbol"] && character.skills["fbol"].auto,
      user_message: "You fire a fire bolt at {{target}} for {{amount}} damage!",
      target_message: "{{user}} fire a fire bolt at you for {{amount}} damage!",
      spectator_message: "{{user}} fires a fire bolt at {{target}} for {{amount}} damage!",
      traits: %{
        "Damage" => [
          %{
            damage_type: "Fire",
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
      <span style="color: lime">Fire Bolt</span>
      This spell shoots a bolt of pure flame at the target, causing minor damage.
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Fire Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= Skill.max_level() do
      "\nNext Skill Level: #{level}\nFire Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
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
