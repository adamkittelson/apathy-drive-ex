defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["sk", "skills"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Skill                Rank</span></p>"
    )

    character.skills
    |> Enum.each(fn {_command, skill} ->
      name =
        skill.name
        |> to_string
        |> String.pad_trailing(20)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan item-name'>#{name} #{skill.level}<span class='item tooltip'>#{skill.module.tooltip(character, skill.skill)}</span></span></p>"
      )
    end)

    room
  end
end
