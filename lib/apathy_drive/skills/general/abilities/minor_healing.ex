defmodule ApathyDrive.Skills.MinorHealing do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "heal",
      command: "mihe",
      targets: "self or single",
      name: "minor healing",
      attributes: ["willpower"],
      mana: mana(),
      auto: !!get_in(character, [:skills, "mihe", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast minor healing on {{target}}, healing {{amount}} damage!",
      target_message: "{{user}} casts minor healing on you, healing {{amount}} damage!",
      spectator_message: "{{user}} casts minor healing on {{target}}, healing {{amount}} damage!",
      traits: %{
        "Heal" => %{
          "min" => min_healing(level),
          "max" => max_healing(level)
        }
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Minor Healing</span>
      Heals minor damage for a small mana cost.
      Attribute(s): #{attributes()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Heals: #{min_healing(level)}-#{max_healing(level)}
      Mana Cost: #{mana()}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Skill Level: #{level}\nHeals: #{min_healing(level)}-#{max_healing(level)}\nMana Cost: #{mana()}"
    end
  end

  defp min_healing(level) do
    trunc(2 + level / 2)
  end

  defp max_healing(level) do
    trunc(7 + 2 * level / 2)
  end

  defp mana(), do: 2
end
