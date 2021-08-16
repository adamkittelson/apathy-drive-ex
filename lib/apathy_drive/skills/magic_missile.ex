defmodule ApathyDrive.Skills.MagicMissile do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "attack",
      command: "mmis",
      targets: "monster or single",
      energy: 500,
      name: "magic missile",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "mmis", :auto]),
      user_message: "You fire a magic missile at {{target}} for {{amount}} damage!",
      target_message: "{{user}} fires a magic missile at you for {{amount}} damage!",
      spectator_message: "{{user}} fires a magic missile at {{target}} for {{amount}} damage!",
      traits: %{
        "Damage" => [
          %{
            damage_type: "Magical",
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
      <span style="color: lime">Magic Missile</span>
      This spell shoots a shimmering dart of pure mana at the target, causing minor damage.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Magic Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Magic Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 2

  defp min_damage(level) do
    trunc(1 + level * 2.2)
  end

  defp max_damage(level) do
    trunc(4 + level * 2.8)
  end
end
