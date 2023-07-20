defmodule ApathyDrive.Skills.MagicMissile do
  alias ApathyDrive.{Ability, Mobile}
  use ApathyDrive.Skill

  def ability(character) do
    level = skill_level(character)

    %Ability{
      kind: "attack",
      command: "mmis",
      targets: "monster or single",
      energy: 0,
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
        "Color" => "magenta",
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Magic Missile</span>
      This spell shoots a shimmering dart of pure mana at the target, causing minor damage.
      Attribute(s): #{attributes()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Magic Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Magic Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
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
