defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Level, Mobile}

  def keywords, do: ["sk", "skills"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p><span class='dark-magenta'>Level Progress Skill</span></p>")

    character.skills
    |> Enum.map(fn {name, %{level: level} = skill} ->
      exp = trunc(skill.experience)
      currentLevel = Level.exp_at_level(level)
      tolevel = Level.exp_at_level(level + 1)
      percent = ((exp - currentLevel) / (tolevel - currentLevel) * 100) |> round

      level =
        level
        |> to_string
        |> String.pad_leading(5)

      percent =
        percent
        |> to_string
        |> String.pad_leading(5)

      {level, percent, name}
    end)
    |> Enum.sort()
    |> Enum.reverse()
    |> Enum.each(fn {level, percent, name} ->
      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{level} #{percent}%    #{name}</span></p>"
      )
    end)

    room
  end
end
