defmodule ApathyDrive.Skills.MinorHealing do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  def ability(%Character{} = character) do
    level = skill_level(character)

    level
    |> ability()
    |> Map.put(:auto, character.skills["mihe"].auto)
  end

  def ability(level) do
    %Ability{
      kind: "heal",
      command: "mihe",
      targets: "self or single",
      energy: 600,
      name: "minor healing",
      attributes: ["willpower"],
      mana: mana(),
      spell?: true,
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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Minor Healing</span>
      Heals minor damage for a small mana cost.
      #{current_skill_level(character)}#{next_skill_level(character)}
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

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= Skill.max_level() do
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
