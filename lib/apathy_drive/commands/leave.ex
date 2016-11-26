defmodule ApathyDrive.Commands.Leave do
  use ApathyDrive.Command
  alias ApathyDrive.Character

  def keywords, do: ["leave"]

  def execute(%Room{} = room, %Character{leader: nil} = character, _arguments) do
    Mobile.send_scroll(character, "<p>You are not currently in a party.</p>")
    room
  end

  def execute(%Room{} = room, %Character{leader: ref, ref: ref} = character, _arguments) do
    room.mobiles
    |> Map.values
    |> Enum.reduce(room, fn
         %Character{leader: ^ref, ref: ^ref} = leader, updated_room ->
           Mobile.send_scroll(leader, "<p><span class='blue'>You disband the party.</span></p>")
           updated_room
           |> put_in([:mobiles, ref, :leader], nil)
           |> put_in([:mobiles, ref, :invitees], [])
         %Character{leader: ^ref, ref: char_ref} = char, updated_room ->
           Mobile.send_scroll(char, "<p><span class='blue'>#{character.name} has disbanded the party.</span></p>")
           put_in(updated_room.mobiles[char_ref].leader, nil)
         _, updated_room ->
           updated_room
       end)
  end

  def execute(%Room{} = room, %Character{leader: ref} = character, _arguments) do
    leader = room.mobiles[ref]
    Mobile.send_scroll(character, "<p><span class='blue'>You are no longer following #{leader.name}.</span></p>")
    room = put_in(room.mobiles[character.ref].leader, nil)

    if party_count(room, ref) < 2 and leader.invitees == [] do
      put_in(room.mobiles[ref].leader, nil)
    else
      room
    end
  end

  defp party_count(room, leader_ref) do
    room.mobiles
    |> Map.values
    |> Enum.count(& &1.leader == leader_ref)
  end
end
