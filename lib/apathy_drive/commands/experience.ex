defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "\n<p><span class='white'>Class:</span></p>"
    )

    exp = character.class.experience
    level = character.level

    to_level = Level.exp_at_level(level, character.class.class.exp_modifier / 100)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Class:</span> <span class='dark-cyan'>#{
        character.class.class.name
        |> String.pad_trailing(10)
      }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
        level
        |> to_string
        |> String.pad_trailing(3)
      }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
        max(0, to_level - exp)
      }</p>"
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
          |> String.pad_trailing(10)
        }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
          level
          |> to_string
          |> String.pad_trailing(3)
        }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
          to_level - exp
        }</p>"
      )
    end)

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
    |> Enum.each(fn {name, skill} ->
      exp = skill.experience
      level = skill.level

      to_level = Level.exp_at_level(level + 1, 1.0)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Skill:</span> <span class='dark-cyan'>#{
          name
          |> to_string
          |> String.pad_trailing(skill_pad)
        }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
          level
          |> to_string
          |> String.pad_trailing(3)
        }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
          to_level - exp
        }</p>"
      )
    end)

    room
  end
end
