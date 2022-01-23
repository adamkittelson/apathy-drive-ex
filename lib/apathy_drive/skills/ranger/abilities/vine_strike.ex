defmodule ApathyDrive.Skills.VineStrike do
  alias ApathyDrive.{Ability, Mobile, Skill}
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.NatureMagic

  def ability(character, level \\ nil) do
    level = level || skill_level(character)

    %Ability{
      kind: "attack",
      command: "vine",
      targets: "monster or single",
      name: "vine strike",
      attributes: @skill.ability(character).attributes,
      mana: mana(level),
      spell?: true,
      energy: 0,
      cast_time: 2500,
      auto: !!get_in(character, [:skills, "vine", :auto]),
      user_message: "You cast vine strike at {{target}} for {{amount}} damage!",
      target_message: "{{user}} casts vine strike at you for {{amount}} damage!",
      spectator_message: "{{user}} casts vine strike at {{target}} for {{amount}} damage!",
      traits: %{
        "Damage" => [
          %{
            damage_type: "Cutting",
            min: min_damage(character, level),
            max: max_damage(character, level)
          }
        ]
      }
    }
  end

  def help(character, skill) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character, skill)}</p>")
  end

  def tooltip(character, skill) do
    """
      <span style="color: lime">Vine Strike</span>
      With this spell, the caster throws out a thorny vine to lash their foe.
      Skill: #{@skill.name()}
      #{current_skill_level(character)}#{next_skill_level(character, skill)}
    """
  end

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Cutting Damage: #{min_damage(character, level)}-#{max_damage(character, level)}
      Mana Cost: #{mana(level)}
      """
    end
  end

  defp next_skill_level(character, skill) do
    level = skill_level(character) + 1

    if level <= skill.max_level do
      "\nNext Ability Level: #{level}\n#{required_level(character.level)}Cutting Damage: #{min_damage(character, level)}-#{max_damage(character, level)}\nMana Cost: #{mana(level)}"
    end
  end

  defp mana(level), do: level

  defp min_damage(character, level) do
    trunc(4 * level * @skill.skill_level(character) / 100)
  end

  defp max_damage(character, level) do
    # 9-12
    trunc(9 * level * (@skill.skill_level(character) / 100))
  end
end
