defmodule ApathyDrive.Skills.IceBolt do
  alias ApathyDrive.{Ability, Mobile}

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

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Ice Bolt</span>
      This spell shoots a bolt of pure ice at the target, causing minor damage.
      #{current_skill_level(character)}#{next_skill_level(character)}
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

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= 20 do
      "\nNext Skill Level: #{level}\nCold Damage: #{min_damage(level)}-#{max_damage(level)}\nMana Cost: #{
        mana(level)
      }"
    end
  end

  defp mana(_level), do: 2

  defp min_damage(level) do
    trunc(1 + level * 2.2)
  end

  defp max_damage(level) do
    trunc(4 + level * 2.8)
  end

  defp skill_level(character) do
    character.skills
    |> Map.values()
    |> Enum.find(&(&1.name == "Ice Bolt"))
    |> case do
      %{level: level} ->
        level

      _ ->
        0
    end
  end
end
