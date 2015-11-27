defmodule Commands.Abilities do
  use ApathyDrive.Command

  def keywords, do: ["abilities", "spells"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='white'>You have the following abilities:</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>")
    display_abilities(mobile)
  end

  def display_abilities(mobile) do
    mobile
    |> Mobile.ability_list
    |> Enum.each(fn(%{"name" => name, "command" => command} = ability) ->
         mana_cost = ability["mana_cost"]
                     |> to_string
                     |> String.ljust(6)

         command = command
                   |> to_string
                   |> String.ljust(8)

         Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{mana_cost} #{command} #{name}</span></p>")
       end)
  end

end
