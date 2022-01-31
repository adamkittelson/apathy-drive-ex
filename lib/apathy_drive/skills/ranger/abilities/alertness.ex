defmodule ApathyDrive.Skills.Alertness do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.NatureMagic

  def ability(%Character{} = character) do
    level = skill_level(character)

    %Ability{
      kind: "blessing",
      command: "aler",
      targets: "self or single",
      name: "alertness",
      attributes: @skill.ability(character).attributes,
      mana: mana(level),
      duration: duration(character, level),
      auto: !!get_in(character, [:skills, "aler", :auto]),
      spell?: true,
      cast_time: 2500,
      energy: 0,
      user_message: "You cast alertness on {{target}}!",
      target_message: "{{user}} casts alertness on {{target}}!",
      spectator_message: "{{user}} casts alertness on {{target}}!",
      traits: %{
        "RemoveMessage" => "The effects of alertness wear off!",
        "StatusMessage" => "You feel perceptive!",
        "Perception" => 10
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Alertness</span>
      Increases the target's perception, making them better able to detect incoming attacks and hidden items or exits.
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
      Perception: +10
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
    trunc(240 * level * @skill.skill_level(character) / 100)
  end

  defp mana(level), do: 2 + 2 * level
end
