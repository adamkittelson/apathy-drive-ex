defmodule ApathyDrive.Commands.Join do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, TimerManager}

  def keywords, do: ["join", "follow"]

  def execute(%Room{} = room, %Character{ref: ref} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Character{name: name, invitees: invitees} = target ->
        if ref in invitees do
          room
          |> Room.update_mobile(character.ref, fn(character) ->
               Mobile.send_scroll(character, "<p>You are now following #{target.name}</p>")
               character
               |> put_in([:leader], target.ref)
             end)
          |> Room.update_mobile(target.ref, fn(target) ->
               Mobile.send_scroll(target, "<p>#{character.name} started to follow you</p>")
               target
               |> update_in([:invitees], &List.delete(&1, ref))
             end)
        else
          Mobile.send_scroll(character, "<p><span class='red'>You must be invited first!</span></p>")
          room
        end
      mobile ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here!</p>")
        room
    end
  end
end
