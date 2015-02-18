defmodule Commands.Abilities do
  use ApathyDrive.Command

  def keywords, do: ["abilities", "spells"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You must be possessing a monster to have abilities.</p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    monster
    |> Monster.send_scroll("<p><span class='white'>You have the following abilities:</span></p>")
    |> Monster.send_scroll("<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>")
    |> display_abilities

    monster
  end

  def display_abilities(%Monster{} = monster) do
    monster.abilities
    |> Enum.filter(fn(%Ability{command: nil}) ->
                       false
                     (%Ability{command: _}) ->
                       true
       end)
    |> Enum.each(fn(%Ability{name: name, command: command, properties: properties}) ->
         mana_cost = properties["mana_cost"]
                     |> to_string
                     |> String.ljust(6)

         command = command
                   |> to_string
                   |> String.ljust(8)

         Monster.send_scroll(monster, "<p><span class='dark-cyan'>#{mana_cost} #{command} #{name}</span></p>")
       end)
  end

end
