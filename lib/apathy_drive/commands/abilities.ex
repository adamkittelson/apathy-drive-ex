defmodule ApathyDrive.Commands.Abilities do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, Mobile}

  def keywords, do: ["abilities", "spells"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='white'>You have the following abilities:</span></p>")
    Mobile.send_scroll(character, "<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>")
    display_abilities(character)
    room
  end

  def display_abilities(%Character{} = character) do
    character.abilities
    |> Map.values
    |> Enum.each(fn(%{name: name, command: command, mana: _mana} = spell) ->
         mana_cost =
           spell
           |> Ability.mana_cost_at_level(character.level)
           |> to_string
           |> String.pad_trailing(6)

         command =
           command
           |> to_string
           |> String.pad_trailing(8)

         Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{mana_cost} #{command} #{name}</span></p>")
       end)
  end

end
