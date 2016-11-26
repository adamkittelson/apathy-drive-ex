defmodule ApathyDrive.Commands.Party do
  use ApathyDrive.Command
  alias ApathyDrive.Character

  def keywords, do: ["party", "par"]

  # not in a party
  def execute(%Room{} = room, %Character{leader: nil, invitees: []} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='red'>You are not in a party at the present time.</span></p>")
    show_party_member(character, character, "red")
    room
  end

  # party leader
  def execute(%Room{} = room, %Character{ref: ref, leader: ref, invitees: invitees} = character, _arguments) do
    show_party_members(character, room)
    room
  end

  # party follower
  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='cyan'>You are following Shaitan.</span></p>")
    show_party_members(character, room)
    room
  end

  defp show_party_members(%Character{} = character, room) do
    Mobile.send_scroll(character, "<p>The following people are in your travel party:</p>")
    leader = room.mobiles[character.leader]

    room.mobiles
    |> Map.values
    |> Enum.filter(& Map.get(&1, :leader) == character.leader)
    |> Enum.each(&show_party_member(character, &1, "grey"))

    leader.invitees
    |> Enum.uniq
    |> Enum.each(&show_invitee(character, room.mobiles[&1]))
  end

  defp show_invitee(character, nil), do: :noop
  defp show_invitee(character, invitee) do
    data = %{
      name: String.ljust(invitee.name, 13),
      class: String.ljust("(#{invitee.class})", 13)
    }

    Mobile.send_scroll(character, "<p>#{data.name}#{data.class}[Invited]</p>")
  end

  defp show_party_member(character, member, color) do
    data = %{
      name: String.ljust(member.name, 13),
      class: String.ljust("(#{member.class})", 13),
      hp: trunc(member.hp * 100) |> to_string |> String.rjust(3),
      mana: trunc(member.mana * 100) |> to_string |> String.rjust(3)
    }

    Mobile.send_scroll(character, "<p><span class='#{color}'>#{data.name}#{data.class}[M:#{data.mana}%] [H:#{data.hp}%]</span></p>")
  end
end
