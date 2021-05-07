defmodule ApathyDrive.Commands.Status do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Level, Mobile}

  def keywords, do: ["st", "stat", "status"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    status(character)

    room
  end

  def status(character) do
    drain_rate = Character.drain_rate(character.level)

    exp_to_level = Level.exp_to_next_level(character.level, character.experience)
    time_to_level = trunc(exp_to_level / drain_rate)

    ttl = ApathyDrive.Enchantment.formatted_time_left(time_to_level)

    buffer_time =
      ApathyDrive.Enchantment.formatted_time_left(trunc(character.exp_buffer / drain_rate))

    Mobile.send_scroll(
      character,
      "<p><span class='cyan'>experience:</span> <span class='white'>#{trunc(exp_to_level)} (#{ttl})</span> " <>
        "<span class='cyan'>mind:</span> #{mind(character)}</span> <span class='white'>(#{
          buffer_time
        })</span>"
    )
  end

  def mind(character) do
    max_buffer = character.max_exp_buffer
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
