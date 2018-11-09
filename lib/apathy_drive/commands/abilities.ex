defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{ClassAbility, Character, Mobile}

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    number = map_size(character.abilities)
    max = Character.max_active_abilities(character)

    Mobile.send_scroll(
      character,
      "<p><span class='white'>You have the following abilities activated (#{number}/#{max}):</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>"
    )

    display_abilities(character)

    Mobile.send_scroll(
      character,
      "<br/><br/><p><span class='white'>You have the following abilities available:</span></p>"
    )

    display_class_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    character.abilities
    |> Map.values()
    |> Enum.each(fn %{name: name, command: command, mana: mana} = _ability ->
      mana_cost = String.pad_trailing(to_string(mana), 6)

      command =
        command
        |> to_string
        |> String.pad_trailing(8)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{mana_cost} #{command} #{name}</span></p>"
      )
    end)
  end

  def display_class_abilities(%Character{} = character) do
    abilities =
      character.class_id
      |> ClassAbility.abilities_at_level(character.level)
      |> Enum.sort_by(& &1.level)
      |> Enum.map(& &1.ability.name)
      |> Enum.join(", ")

    Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{abilities}</span></p>")
  end
end
