defmodule ApathyDrive.Commands.System.Ability do
  alias ApathyDrive.{Ability, AbilityTrait, Mobile, Room}

  def execute(%Room{} = room, character, ["help" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")
    case Ability.match_by_name(ability_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
      %Ability{} = ability ->
        Mobile.send_scroll(character, "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>")
        Mobile.send_scroll(character, "<p>#{ability.name}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.description}</p>")
        Mobile.send_scroll(character, "\n\n<p>Targets: #{ability.targets}</p>")
        if ability.mana && ability.mana > 0 do
          Mobile.send_scroll(character, "<p>Base Mana: #{ability.mana}</p>")
          Mobile.send_scroll(character, "<p>Mana Cost: #{Ability.mana_cost_at_level(ability, character.level)}</p>")
        end
        if ability.duration_in_ms && ability.duration_in_ms > 0 do
          Mobile.send_scroll(character, "<p>Duration: #{div(ability.duration_in_ms, 1000)} seconds</p>")
        end
        Mobile.send_scroll(character, "\n\n<p>Messages:</p>")
        Mobile.send_scroll(character, "<p>    #{ability.user_message}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.target_message}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.spectator_message}</p>")

        traits = AbilityTrait.load_traits(ability.id)

        Mobile.send_scroll(character, "\n\n<p>Traits:</p>")
        Enum.each(traits, fn
          {name, nil} ->
            Mobile.send_scroll(character, "<p>  #{name}</p>")
          {name, value} ->
            Mobile.send_scroll(character, "<p>  #{name}: #{inspect value}</p>")
        end)

        Mobile.send_scroll(character, "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>")
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)
    end
    room
  end

end