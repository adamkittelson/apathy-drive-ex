defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{
        String.pad_trailing(character.name, 10)
      }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{character.level}</span>"
    )

    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.each(fn attribute ->
      exp = Map.get(character, :"#{attribute}_experience")
      level = character.attribute_levels[attribute]

      current = Level.exp_at_level(level, 1.0)
      to_level = Level.exp_at_level(level + 1, 1.0)

      percent = ((exp - current) / (to_level - current) * 100) |> trunc

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Stat:</span> <span class='dark-cyan'>#{
          attribute
          |> to_string
          |> String.pad_trailing(10)
        }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
          level
          |> to_string
          |> String.pad_trailing(3)
        }</span> <span class='dark-green'>Progress:</span> <span class='dark-cyan'>#{percent}%</p>"
      )
    end)

    room
  end
end
