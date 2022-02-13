defmodule ApathyDrive.Skills.LightningBolt do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def prereq(), do: ApathyDrive.Skills.MagicMissile

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "lbol",
      targets: "monster or single",
      name: "lightning bolt",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      energy: 0,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "lbol", :auto]),
      user_message: "You fire a lightning bolt at {{target}} for {{amount}} damage!",
      target_message: "{{user}} fire a lightning bolt at you for {{amount}} damage!",
      spectator_message: "{{user}} fires a lightning bolt at {{target}} for {{amount}} damage!",
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
      <span style="color: lime">Lightning Bolt</span>
      This spell unleashes a blast of blue-white lightning to assault a foe.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Electricity Damage: #{min_damage(level)}-#{max_damage(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Electricity Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 5

  defp min_damage(_level) do
    16
  end

  defp max_damage(level) do
    # 28-42
    trunc(26 + level * 2.75)
  end
end
