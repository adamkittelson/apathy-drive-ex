defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Class, Mobile, Level, Repo}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "\n<p><span class='white'>Character:</span></p>"
    )

    level = character.level

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
        level
        |> to_string
        |> String.pad_trailing(12)
      }</span>"
    )

    if Enum.any?(character.classes) do
      Mobile.send_scroll(
        character,
        "\n<p><span class='white'>Classes:</span></p>"
      )

      Enum.each(character.classes, fn character_class ->
        class = Repo.get(Class, character_class.class_id)

        exp = max(0, trunc(ApathyDrive.Commands.Train.required_experience(character, class.id)))

        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Class:</span> <span class='dark-cyan'>#{
            String.pad_trailing(class.name, 12)
          }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{
            character_class.level
            |> to_string
            |> String.pad_trailing(3)
          }</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>#{
            exp
          }</p>"
        )
      end)
    end

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
        }</p>"
      )
    end)

    room
  end
end
