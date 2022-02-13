defmodule ApathyDrive.Skills.MajorHealing do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  def prereq(), do: ApathyDrive.Skills.MinorHealing

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "heal",
      command: "mahe",
      targets: "self or single",
      name: "major healing",
      attributes: ["willpower"],
      mana: mana(),
      auto: !!get_in(character, [:skills, "mahe", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast major healing on {{target}}, healing {{amount}} damage!",
      target_message: "{{user}} casts major healing on you, healing {{amount}} damage!",
      spectator_message: "{{user}} casts major healing on {{target}}, healing {{amount}} damage!",
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
      <span style="color: lime">Major Healing</span>
      Heals major damage for a moderate mana cost.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Heals: #{min_healing(level)}-#{max_healing(level)}
      Mana Cost: #{mana()}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Heals: #{min_healing(level)}-#{max_healing(level)}\nMana Cost: #{mana()}"
    end
  end

  defp min_healing(level) do
    # 8 - 18
    trunc(6 + level * 2)
  end

  defp max_healing(level) do
    # 16 - 40
    trunc(12 + level * 4.75)
  end

  defp mana(), do: 6
end
