defmodule ApathyDrive.Commands.Experience do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Level}

  def keywords, do: ["exp", "experience"]

  def execute(%Room{} = room, %Character{} = character, []) do
    training =
      if Character.max_level(character) > character.level do
        "<span class='yellow'>Ready to train to next level!</span>"
      else
        ""
      end

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{
        String.pad_trailing(character.name, 10)
      }</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>#{character.level}</span>   #{
        training
      }"
    )

    [:strength, :agility, :intellect, :willpower, :health, :charm]
    |> Enum.each(fn attribute ->
      exp = Map.get(character, :"#{attribute}_experience")
      level = character.attribute_levels[attribute]

      to_level = Level.exp_at_level(level + 1, 1.0)

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

    room
  end
end
