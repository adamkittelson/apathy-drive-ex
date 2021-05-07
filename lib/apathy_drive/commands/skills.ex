defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["sk", "skills"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p><span class='dark-magenta'>Skill      Level</span></p>")

    character.skills
    |> Enum.each(fn {name, skill} ->
      name =
        name
        |> to_string
        |> String.pad_trailing(10)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan item-name'>#{name} #{skill.level}<span class='item tooltip'>#{
          skill.module.tooltip(character)
        }</span></span></p>"
      )
    end)

    room
  end
end
