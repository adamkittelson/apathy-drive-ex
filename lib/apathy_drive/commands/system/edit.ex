defmodule ApathyDrive.Commands.System.Edit do
  alias ApathyDrive.{Ability, Mobile, Room}

  def execute(%Room{} = room, character, ["ability" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    case Ability.match_by_name(ability_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
        room
      %Ability{} = ability ->
        Mobile.send_scroll(character, "<p>You are now editing #{ability.name}.</p>")

        Room.update_mobile(room, character.ref, fn character ->
          character
          |> Map.put(:editing, ability)
          |> Mobile.update_prompt
        end)
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)
        room
    end
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end


end
