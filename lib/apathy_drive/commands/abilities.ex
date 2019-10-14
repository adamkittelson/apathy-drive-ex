defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, Mobile}
  require Ecto.Query

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You know the following abilities:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Auto   Mana   Command  Ability Name</span></p>"
    )

    display_abilities(character)
    display_enchantments(character)
    display_scroll_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    character.abilities
    |> Map.values()
    |> Enum.sort_by(& &1.level)
    |> Enum.reject(&(&1.kind == "long-term"))
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
  end

  def display_enchantments(%Character{} = character) do
    abilities =
      character.abilities
      |> Map.values()
      |> Enum.sort_by(& &1.level)
      |> Enum.filter(&(&1.kind == "long-term"))

    if Enum.any?(abilities) do
      Mobile.send_scroll(
        character,
        "<br/><br/><p><span class='white'>You know the following enchantments:</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-magenta'>Command  Enchantment Name</span></p>"
      )

      abilities
      |> Enum.each(fn %{name: name, command: command} ->
        command =
          command
          |> to_string
          |> String.pad_trailing(8)

        Mobile.send_scroll(
          character,
          "<p><span class='dark-cyan'>#{command} #{name}</span></p>"
        )
      end)
    end
  end

  def display_scroll_abilities(%Character{} = character) do
    abilities =
      character
      |> scrolls()
      |> Enum.map(& &1.traits["Learn"])
      |> Enum.map(&%{name: &1.name, command: &1.command, mana: &1.mana})
      |> Enum.uniq()

    if Enum.any?(abilities) do
      Mobile.send_scroll(
        character,
        "<br/><br/><p><span class='white'>You have usable scrolls for the following abilities:</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>"
      )

      abilities
      |> Enum.each(fn %{name: name, command: command, mana: _mana} = _ability ->
        mana_cost = String.pad_trailing(to_string(0), 6)

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
  end

  def scrolls(%Character{inventory: inventory} = character) do
    Enum.filter(
      inventory,
      &(&1.type == "Scroll" && !is_nil(&1.traits["Learn"]) &&
          !Item.too_powerful_for_character?(character, &1))
    )
  end

  def emoji(true), do: "✅"
  def emoji(_), do: "❌"
end
