defmodule ApathyDrive.Skills.Starlight do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.NatureMagic

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "blessing",
      command: "star",
      targets: "self or single",
      name: "starlight",
      attributes: @skill.ability(character).attributes,
      mana: mana(level),
      duration: duration(character, level),
      auto: !!get_in(character, [:skills, "star", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast starlight!",
      target_message: "{{user}} casts starlight!",
      spectator_message: "{{user}} casts starlight!",
      traits: %{
        "RemoveMessage" => "Your starlight spell fades away.",
        "StatusMessage" => "You are surrounded by a shimmering light!",
        "Light" => 175
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Starlight</span>
      This spells focuses the power of the stars to illuminate an area.
      Skill: #{@skill.name()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability(character)) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Duration: #{Float.round(duration(character, level) / 60, 2)} minutes
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Ability Level: #{level}\nDuration: #{Float.round(duration(character, level) / 60, 2)} minutes\nMana Cost: #{mana(level)}"
    end
  end

  defp duration(character, level) do
    trunc(320 * level * @skill.skill_level(character) / 100)
  end

  defp mana(level), do: 4 * level
end
