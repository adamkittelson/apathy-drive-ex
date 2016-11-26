defmodule ApathyDrive.Commands.Leave do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Party}

  def keywords, do: ["leave"]

  def execute(%Room{} = room, %Character{leader: ref, ref: ref} = character, _arguments) do
    room.mobiles
    |> Map.values
    |> Enum.reduce(room, fn
         %Character{leader: ^ref, ref: ^ref} = leader, updated_room ->
           Mobile.send_scroll(leader, "<p><span class='blue'>You disband the party.</span></p>")
           updated_room
           |> put_in([:mobiles, ref, :invitees], [])
         %Character{leader: ^ref, ref: char_ref} = char, updated_room ->
           Mobile.send_scroll(char, "<p><span class='blue'>#{character.name} has disbanded the party.</span></p>")
           put_in(updated_room.mobiles[char_ref].leader, char_ref)
         _, updated_room ->
           updated_room
       end)
  end

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    leader = Party.leader(room, character)
    Mobile.send_scroll(character, "<p><span class='blue'>You are no longer following #{leader.name}.</span></p>")
    put_in(room.mobiles[character.ref].leader, character.ref)
  end

end
