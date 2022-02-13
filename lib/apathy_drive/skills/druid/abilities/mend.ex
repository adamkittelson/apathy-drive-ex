defmodule ApathyDrive.Skills.Mend do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.Balance

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "heal",
      command: "mend",
      targets: "self or single",
      name: "mend",
      attributes: @skill.ability(character).attributes,
      mana: mana(level),
      auto: !!get_in(character, [:skills, "mend", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast mend on {{target}}, healing {{amount}} damage!",
      target_message: "{{user}} casts mend on you, healing {{amount}} damage!",
      spectator_message: "{{user}} casts mend on {{target}}, healing {{amount}} damage!",
      traits: %{
        "Heal" => %{
          "min" => min_healing(character, level),
          "max" => max_healing(character, level)
        }
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Mend</span>
      By casting this spell, the wounds of the target are healed.
      Skill: #{@skill.name()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  def casting_skill, do: @skill

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Heals: #{min_healing(character, level)}-#{max_healing(character, level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\nHeals: #{min_healing(character, level)}-#{max_healing(character, level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp min_healing(character, level) do
    trunc(3 * level * @skill.skill_level(character) / 100)
  end

  defp max_healing(character, level) do
    trunc(13 * level * @skill.skill_level(character) / 100)
  end

  defp mana(level), do: 2 + 2 * level
end
