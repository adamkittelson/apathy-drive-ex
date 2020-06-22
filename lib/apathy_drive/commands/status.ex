defmodule ApathyDrive.Commands.Status do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["st", "stat", "status"]

  def execute(%Room{} = room, %Character{} = character, []) do
    status(character)

    room
  end

  def status(character) do
    hp = Character.hp_at_level(character, character.level)
    max_hp = Mobile.max_hp_at_level(character, character.level)

    mp = Character.mana_at_level(character, character.level)
    max_mp = Mobile.max_mana_at_level(character, character.level)

    {exp, time_to_level} =
      if Enum.any?(character.classes) do
        character.classes
        |> Enum.map(fn character_class ->
          exp_to_level =
            ApathyDrive.Commands.Train.required_experience(
              character,
              character_class.class_id,
              character_class.level + 1
            )
            |> trunc()

          drain_rate = Character.drain_rate(character_class.level)

          {exp_to_level, trunc(exp_to_level / drain_rate)}
        end)
        |> Enum.max_by(fn {_exp_to_level, time_to_level} -> time_to_level end)
      else
        {0, 0}
      end

    ttl = ApathyDrive.Enchantment.formatted_time_left(time_to_level)

    Mobile.send_scroll(
      character,
      "<p><span class='cyan'>hp:</span> <span class='white'>#{hp}/#{max_hp}</span> " <>
        "<span class='cyan'>mana:</span> <span class='white'>#{mp}/#{max_mp}</span> " <>
        "<span class='cyan'>experience:</span> <span class='white'>#{exp} (#{ttl})</span> " <>
        "<span class='cyan'>mind:</span> #{mind(character)}"
    )
  end

  def mind(character) do
    max_buffer = Character.max_exp_buffer(character)
    buffer = character.exp_buffer

    percent = buffer / max_buffer

    cond do
      percent < 0.05 ->
        "<span class='white'>clear</span>"

      percent < 0.25 ->
        "<span class='white'>almost clear</span>"

      percent < 0.5 ->
        "<span class='white'>slightly fuzzy</span>"

      percent < 0.75 ->
        "<span class='white'>clouded</span>"

      percent < 0.90 ->
        "<span class='white'>very fuzzy</span>"

      :else ->
        "<span class='magenta'>full of facts</span>"
    end
  end
end
