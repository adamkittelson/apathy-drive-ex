defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}
  require Ecto.Query

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    display_abilities(character)
    display_auto_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You know the following abilities:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Command  Ability Name</span>             <span class='dark-magenta'>Command  Ability Name</span></p>"
    )

    character.abilities
    |> Map.values()
    |> Enum.sort_by(& &1.name)
    |> Enum.reject(&(&1.auto == true))
    |> Enum.map(&format_ability/1)
    |> Enum.chunk_every(2)
    |> Enum.each(fn
      [ability1, ability2] ->
        Mobile.send_scroll(
          character,
          "<p>#{ability1}#{ability2}</p>"
        )

      [ability] ->
        Mobile.send_scroll(character, "<p>#{ability}</p>")
    end)
  end

  def format_ability(%{name: name, command: command}) do
    command =
      command
      |> to_string
      |> String.pad_trailing(8)

    name = String.pad_trailing(name, 25)

    "<span class='dark-cyan'>#{command} #{name}</span>"
  end

  def display_auto_abilities(%Character{} = character) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You will automatically cast the following abilities:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Command  Ability Name</span>             <span class='dark-magenta'>Command  Ability Name</span></p>"
    )

    character.abilities
    |> Map.values()
    |> Enum.sort_by(& &1.name)
    |> Enum.filter(&(&1.auto == true))
    |> Enum.map(&format_ability/1)
    |> Enum.chunk_every(2)
    |> Enum.each(fn
      [ability1, ability2] ->
        Mobile.send_scroll(
          character,
          "<p>#{ability1}#{ability2}</p>"
        )

      [ability] ->
        Mobile.send_scroll(character, "<p>#{ability}</p>")
    end)
  end

  def emoji(true), do: "✅"
  def emoji(_), do: "❌"
end
