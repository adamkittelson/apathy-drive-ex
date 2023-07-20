defmodule ApathyDrive.Skills.CureBlindness do
  alias ApathyDrive.{Ability, Character, Mobile}
  use ApathyDrive.Skill

  def prereq(), do: ApathyDrive.Skills.MinorHealing

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "util",
      command: "site",
      targets: "self or single",
      energy: 600,
      name: "cure blindness",
      attributes: ["willpower"],
      mana: mana(level),
      auto: !!get_in(character, [:skills, "site", :auto]),
      spell?: true,
      user_message: "You cast cure blindness on {{target}}!",
      target_message: "{{user}} casts cure blindness upon you!",
      spectator_message: "{{user}} casts cure blindness on {{target}}!",
      traits: %{
        "DispelMagic" => 107
      }
    }
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">#{name()}</span>
      This spell restores sight to those who have been blinded.
      Attribute(s): #{attributes()}
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Cures Blindness
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}#{prereq(character, level)}Cures Blindness\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(_level), do: 12
end
