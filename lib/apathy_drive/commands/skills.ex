defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["sk", "skills"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "\n<p><span class='white'>Skills:</span></p>"
    )

    skill_pad =
      character.skills
      |> Map.keys()
      |> Enum.map(&String.length/1)
      |> Enum.max()

    character.skills
    |> Enum.sort_by(fn {_name, skill} -> skill.id end)
    |> Enum.each(fn {name, skill} ->
      level = skill.level

      if level > 0 do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Skill:</span> <span class='dark-cyan'>#{
            name
            |> to_string
            |> String.pad_trailing(skill_pad + 10)
          }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
            level
            |> to_string
            |> String.pad_trailing(3)
          }</span></p>"
        )
      end
    end)

    room
  end
end
