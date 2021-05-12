defmodule ApathyDrive.Skills.Healing do
  alias ApathyDrive.{Ability, Character, Mobile}

  def ability(%Character{} = character) do
    level = skill_level(character)

    level
    |> ability()
    |> Map.put(:auto, character.skills["heal"].auto)
  end

  def ability(level) do
    %Ability{
      kind: "heal",
      command: "heal",
      targets: "self or single",
      energy: 600,
      name: "healing",
      attributes: ["willpower"],
      mana: mana(level),
      spell?: true,
      user_message: "You cast healing on {{target}}, healing {{amount}} damage!",
      target_message: "{{user}} casts healing on you, healing {{amount}} damage!",
      spectator_message: "{{user}} casts healing on {{target}}, healing {{amount}} damage!",
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
      <span style="color: lime">Healing</span>
      With sufficient faith a priest may heal themselves or an ally of even the most grievous of wounds.
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Skill Level: #{level}
      Heals: #{min_healing(level)}-#{max_healing(level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= 20 do
      "\nNext Skill Level: #{level}\nHeals: #{min_healing(level)}-#{max_healing(level)}\nMana Cost: #{
        mana(level)
      }"
    end
  end

  defp mana(level) do
    trunc(1 + level * 1.15)
  end

  defp min_healing(level) do
    trunc(3 + level * 2.85)
  end

  defp max_healing(level) do
    trunc(8 + level * 6.1)
  end

  defp skill_level(character) do
    character.skills
    |> Map.values()
    |> Enum.find(&(&1.name == "healing"))
    |> case do
      %{level: level} ->
        level

      _ ->
        0
    end
  end
end
