defmodule ApathyDrive.Skills.ChainLightning do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "lbol",
      targets: "full attack area",
      energy: 500,
      name: "chain lightning",
      attributes: ["intellect"],
      mana: mana(level),
      spell?: true,
      auto: !!get_in(character, [:skills, "cbol", :auto]),
      user_message: "Blue lightning streaks out and sears {{target}} for {{amount}} damage!",
      target_message: "Blue lightning streaks out and sears you for {{amount}} damage!",
      spectator_message: "{{user}} fires a lightning bolt at {{target}} for {{amount}} damage!",
      traits: %{
        "PreCastMessage" => "You move your hands in a mystical pattern!",
        "PreCastSpectatorMessage" => "{{user}} moves their hands in a mystical pattern!",
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

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Charged Bolt</span>
      This spell unleashes a blast of blue-white lightning to assault a foe.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
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

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Electricity Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 75

  defp min_damage(level) do
    # 270-330
    trunc(258 + level * 12)
  end

  defp max_damage(level) do
    # 525-660
    trunc(498 + level * 27)
  end
end
