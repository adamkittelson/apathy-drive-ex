defmodule ApathyDrive.Skills.FireBolt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "fbol",
      targets: "monster or single",
      energy: 0,
      name: "fire bolt",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "fbol", :auto]),
      user_message: "You cast a fire bolt at {{target}} for {{amount}} damage!",
      target_message: "{{user}} casts fire bolt at you for {{amount}} damage!",
      spectator_message: "{{user}} cast fire bolt at {{target}} for {{amount}} damage!",
      traits: %{
        "Color" => "magenta",
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

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Fire Bolt</span>
      This spell shoots a bolt of pure flame at the target, causing minor damage.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
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

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Fire Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 2

  defp min_damage(level) do
    # 3-45
    trunc(-5 + level * 8.4)
  end

  defp max_damage(level) do
    # 6-60
    trunc(-4 + level * 10.75)
  end
end
