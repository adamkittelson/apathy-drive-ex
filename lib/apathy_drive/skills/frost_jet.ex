defmodule ApathyDrive.Skills.FrostJet do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "ibol",
      targets: "monster or single",
      energy: 500,
      name: "ice bolt",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      auto: character.skills["ibol"] && character.skills["ibol"].auto,
      user_message: "You fire an ice bolt at {{target}} for {{amount}} damage!",
      target_message: "{{user}} fires an ice bolt at you for {{amount}} damage!",
      spectator_message: "{{user}} fires an ice bolt at {{target}} for {{amount}} damage!",
      traits: %{
        "Damage" => [
          %{
            damage_type: "Cold",
            min: min_damage(level),
            max: max_damage(level)
          }
        ]
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Ice Bolt</span>
      This spell shoots a bolt of pure ice at the target, causing minor damage.
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Cold Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Cold Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 2

  defp min_damage(level) do
    # 14 - 21
    trunc(13 + level * 1.4)
  end

  defp max_damage(level) do
    # 22-32
    trunc(20 + level * 2)
  end
end
