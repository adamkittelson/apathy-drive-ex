defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, Mobile}
  require Ecto.Query

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    display_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    skill_abilities = Ability.skill_abilities(character)

    abilities =
      character.abilities
      |> Map.values()

    abilities = abilities ++ skill_abilities

    if Enum.any?(abilities) do
      Mobile.send_scroll(
        character,
        "<p><span class='white'>You know the following abilities:</span></p>"
      )

      name_width =
        abilities
        |> Enum.max_by(&String.length(&1.name))
        |> Map.get(:name)
        |> String.length()
        |> max(15)

      ability_name = String.pad_trailing("Ability Name", name_width)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-magenta'>Mana   Command  #{ability_name}</span> <span class='dark-magenta'>Mana   Command  Ability Name</span></p>"
      )

      abilities
      |> Enum.sort_by(& &1.name)
      |> Enum.map(&format_ability(&1, name_width))
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
    else
      Mobile.send_scroll(
        character,
        "<p><span class='white'>You don't know any abilities.</span></p>"
      )
    end
  end

  def format_ability(%{name: name, mana: mana, command: command, auto: auto}, name_width) do
    command =
      command
      |> to_string
      |> String.pad_trailing(8)

    mana_cost = String.pad_trailing(to_string(mana), 6)

    name = String.pad_trailing(name, name_width + 1)

    name =
      if auto do
        "<span class='dark-magenta'>#{name}</span>"
      else
        name
      end

    "<span class='dark-cyan'>#{mana_cost} #{command} #{name}</span>"
  end

  def emoji(true), do: "✅"
  def emoji(_), do: "❌"
end
