defmodule Commands.Abilities do
  use Systems.Command

  def keywords, do: ["abilities", "spells"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>You have the following abilities:</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-magenta'>Power  Command  Ability Name</span></p>")

    display_abilities(spirit)
  end

  def execute(spirit, monster, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>You have the following abilities:</span></p>")
    send_message(spirit, "scroll", "<p><span class='dark-magenta'>Mana   Command  Ability Name</span></p>")
    display_abilities(monster)
  end

  def display_abilities(entity) do
    entity
    |> Components.Abilities.value
    |> Enum.map(fn(ability) ->
         Components.Module.value(ability)
       end)
    |> Enum.filter(fn(module) ->
         module.properties(entity)
         |> Map.has_key?(:command)
       end)
    |> Enum.each(fn(module) ->
         properties = module.properties(entity)
         mana_cost = properties[:mana_cost]
                     |> to_string
                     |> String.ljust(6)

         command = properties[:command]
                   |> to_string
                   |> String.ljust(8)

         send_message(entity, "scroll", "<p><span class='dark-cyan'>#{mana_cost} #{command} #{module.name}</span></p>")
       end)
  end

end
