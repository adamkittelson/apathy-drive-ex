defmodule ApathyDrive.Commands.Status do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["st", "stat", "status"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    status(character)

    room
  end

  def status(character) do
    max_level =
      case character.classes do
        [] ->
          1

        classes ->
          classes
          |> Enum.map(& &1.level)
          |> Enum.max()
      end

    classes = character.classes

    target_count = length(classes)

    max_drain_rate = Character.drain_rate(max_level)

    {exp, time_to_level} =
      if Enum.any?(classes) do
        classes
        |> Enum.map(fn character_class ->
          exp_to_level =
            ApathyDrive.Commands.Train.required_experience(
              character,
              character_class.class_id,
              character_class.level + 1
            )

          class_drain_rate = Character.drain_rate(character_class.level)

          drain_rate = min(class_drain_rate, max_drain_rate / target_count)

          {max(0, exp_to_level), max(0, trunc(exp_to_level / drain_rate))}
        end)
        |> Enum.min_by(fn {_exp_to_level, time_to_level} -> time_to_level end, fn -> {0, 0} end)
      else
        {0, 0}
      end

    ttl = ApathyDrive.Enchantment.formatted_time_left(time_to_level)

    buffer_time =
      ApathyDrive.Enchantment.formatted_time_left(trunc(character.exp_buffer / max_drain_rate))

    Mobile.send_scroll(
      character,
      "<p><span class='cyan'>experience:</span> <span class='white'>#{trunc(exp)} (#{ttl})</span> " <>
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
