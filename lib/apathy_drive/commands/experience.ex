defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "\n<p><span class='white'>Character:</span></p>"
    )

    modifier = (100 + character.race.race.exp_modifier) / 100
    level = character.level
    exp = trunc(character.experience)
    tolevel = Level.exp_at_level(level, modifier)
    remaining = tolevel - exp
    percent = (exp / tolevel * 100) |> round

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{exp}</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
        level
      }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
        remaining
      } (#{tolevel}) [#{percent}%]</span></p>"
    )

    Mobile.send_scroll(
      character,
      "\n<p><span class='white'>Attributes:</span></p>"
    )

    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.each(fn attribute ->
      exp = get_in(character, [Access.key!(:race), Access.key!(:"#{attribute}_experience")])
      level = character.attribute_levels[attribute]

      modifier = (100 + character.race.race.exp_modifier) / 100

      to_level = Level.exp_at_level(level + 1, modifier)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Stat:</span> <span class='dark-cyan'>#{
          attribute
          |> to_string
          |> String.pad_trailing(13)
        }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
          level
          |> to_string
          |> String.pad_trailing(3)
        }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
          to_level - exp
        }</span></p>"
      )
    end)

    room
  end
end
