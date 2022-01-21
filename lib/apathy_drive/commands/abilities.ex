defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}
  require Ecto.Query

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    display_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    abilities =
      character.abilities
      |> Map.values()

    abilities = abilities

    if Enum.any?(abilities) do
      Mobile.send_scroll(
        character,
        "<p><span class='white'>You know the following abilities:</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-magenta'>Auto   Mana   Command  Ability Name</span></p>"
      )

      abilities
      |> Enum.sort_by(& &1.level)
      |> Enum.reject(&(&1.kind == "long-term"))
      |> Enum.reject(&(&1.kind == "passive"))
      |> Enum.each(fn %{name: name, command: command, mana: mana, auto: auto} ->
        mana_cost = String.pad_trailing(to_string(mana), 6)

        command =
          command
          |> to_string
          |> String.pad_trailing(8)

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'> #{emoji(auto)}     #{mana_cost} #{command} #{name}</span></p>"
        )
      end)
    else
      Mobile.send_scroll(
        character,
        "<p><span class='white'>You don't know any abilities.</span></p>"
      )
    end
  end

  def emoji(true), do: "✅"
  def emoji(_), do: "❌"
end
