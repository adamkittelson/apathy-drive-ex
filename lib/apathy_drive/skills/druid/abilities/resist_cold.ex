defmodule ApathyDrive.Skills.ResistCold do
  alias ApathyDrive.{Ability, Character, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.Balance
  @id 29

  def ability(%Character{} = character, level \\ nil) do
    level = level || skill_level(character)
    ability = Ability.find(@id)

    ability
    |> Map.put(:mana, mana(ability.mana, level))
    |> Map.put(:duration, duration(ability.duration, character, level))
    |> Map.put(:spell?, true)
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    ability = ability(character)

    """
      <span style="color: lime">#{ability.name}</span>
      #{ability.description}
      Skill: #{@skill.name()}
      Cast Time: #{Float.round(Mobile.cast_time(character, ability) / 1000, 2)} seconds
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  def casting_skill, do: @skill

  defp current_skill_level(character) do
    level = skill_level(character)
    ability = ability(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Resist Cold: #{ability.traits["ResistCold"]}%
      Duration: #{Float.round(ability.duration / 60, 2)} minutes
      Mana Cost: #{mana(ability.mana, level)}
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1
    ability = ability(character, level)

    if level <= max_skill_level(character) do
      """
      \nNext Ability Level: #{level}
      Resist Cold: #{ability.traits["ResistCold"]}%
      Duration: #{Float.round(ability.duration / 60, 2)} minutes
      Mana Cost: #{mana(ability.mana, level)}
      """
    end
  end

  defp duration(duration, character, level) do
    trunc(duration * level * @skill.skill_level(character) / 100)
  end

  defp mana(mana, level), do: trunc(mana / 2 + mana / 2 * level)
end
