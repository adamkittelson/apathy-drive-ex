defmodule ApathyDrive.Skills.Backstab do
  alias ApathyDrive.Mobile
  use ApathyDrive.Skill

  @skill ApathyDrive.Skills.Stealth

  def ability(character) do
    level = skill_level(character)

    attack_ability =
      character
      |> Mobile.attack_ability()

    attack_ability
    |> Map.put(:stealth?, true)
    |> Map.put(:command, "bs")
    |> Map.put(:name, "backstab")
    |> Map.put(:targets, "monster or single")
    |> Map.put(:attributes, @skill.ability(character).attributes)
    |> Map.put(:mana, mana(level))
    |> Map.put(:auto, !!get_in(character, [:skills, "bs", :auto]))
    |> Map.put(:attributes, ["agility"])
    |> Map.put(:cast_time, 2500)
    |> Map.put(:energy, 0)
    |> Map.put(:spell?, false)
    |> Map.put(
      :user_message,
      surprise_message(attack_ability.user_message)
    )
    |> Map.put(
      :target_message,
      surprise_message(attack_ability.target_message)
    )
    |> Map.put(
      :spectator_message,
      surprise_message(attack_ability.spectator_message)
    )
    |> update_in([Access.key!(:traits)], fn traits ->
      traits
      |> update_in(["Damage"], fn damages ->
        Enum.reduce(damages, [], fn
          %{} = damage, damages ->
            modifier = damage_percent(character, level) / 100

            damage =
              damage
              |> Map.put(:min, trunc(damage.min * modifier))
              |> Map.put(:max, trunc(damage.max * modifier))

            [damage | damages]
        end)
      end)
    end)
  end

  def help(character) do
    Mobile.send_scroll(character, "<p class='item'>#{tooltip(character)}</p>")
  end

  def tooltip(character) do
    """
      <span style="color: lime">Backstab</span>
      Surprise an enemy from the shadows with an unexpected attack.
      Skill: #{@skill.name()}
      #{current_skill_level(character)}#{next_skill_level(character)}
    """
  end

  def casting_skill, do: @skill

  defp current_skill_level(character) do
    level = skill_level(character)

    if level > 0 do
      """
      \nCurrent Ability Level: #{level}
      Damage Modifier: #{damage_percent(character, level)}%
      """
    end
  end

  defp next_skill_level(character) do
    level = skill_level(character) + 1

    if level <= max_skill_level(character) do
      "\nNext Ability Level: #{level}\nDamage Modifier: #{damage_percent(character, level)}%"
    end
  end

  defp mana(_level) do
    0
  end

  defp damage_percent(character, level) do
    @skill.skill_level(character) + 100 * level
  end

  defp surprise_message(message) do
    message
    |> String.split(" ")
    |> List.insert_at(1, "surprise")
    |> Enum.join(" ")
  end
end
