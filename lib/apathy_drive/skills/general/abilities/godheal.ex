defmodule ApathyDrive.Skills.Godheal do
  alias ApathyDrive.{Ability, Character, Mobile}
  use ApathyDrive.Skill

  def prereq(), do: ApathyDrive.Skills.GreaterHealing

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "heal",
      command: "gdhe",
      targets: "self or single",
      name: "godheal",
      attributes: ["willpower"],
      mana: mana(),
      auto: !!get_in(character, [:skills, "gdhe", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast godheal on {{target}}, healing {{amount}} damage!",
      target_message: "{{user}} casts godheal on you, healing {{amount}} damage!",
      spectator_message: "{{user}} casts godheal on {{target}}, healing {{amount}} damage!",
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
      <span style="color: lime">#{name()}</span>
      Heals a huge amount of damage at a very high mana cost.
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
    # 46 - 74
    trunc(41 + level * 5.5)
  end

  defp max_healing(level) do
    # 60 - 130
    trunc(46 + level * 14)
  end

  defp mana(), do: 24
end
