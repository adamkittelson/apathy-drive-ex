defmodule ApathyDrive.Commands.Party do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Companion, Party}

  def keywords, do: ["party", "par"]

  # party leader
  def execute(%Room{} = room, %Character{ref: ref, leader: ref, invitees: invitees} = character, _arguments) do
    show_party_members(character, room)
    room
  end

  # party follower
  def execute(%Room{} = room, %Character{} = character, _arguments) do
    leader = Party.leader(room, character)
    Mobile.send_scroll(character, "<p><span class='cyan'>You are following #{leader.name}.</span></p>")
    show_party_members(character, room)
    room
  end

  defp show_party_members(%Character{} = character, room) do
    Mobile.send_scroll(character, "<p>The following people are in your travel party:</p>")

    members = Party.members(room, character)
    invitees = Party.invitees(room, character)

    Enum.each(members, &show_party_member(character, &1, "grey"))
    Enum.each(invitees, &show_invitee(character, room.mobiles[&1]))
  end

  defp show_invitee(character, nil), do: :noop
  defp show_invitee(character, invitee) do
    data = %{
      name: String.ljust(invitee.name, 13),
      class: String.ljust("(#{invitee.class})", 13)
    }

    Mobile.send_scroll(character, "<p>#{data.name}#{data.class}[Invited]</p>")
  end

  defp show_party_member(character, %Character{} = member, color) do
    data = %{
      name: String.ljust(member.name, 13),
      class: String.ljust("(#{member.class})", 13),
      hp: trunc(member.hp * 100) |> to_string |> String.rjust(3),
      mana: trunc(member.mana * 100) |> to_string |> String.rjust(3)
    }

    Mobile.send_scroll(character, "<p><span class='#{color}'>#{data.name}#{data.class}[H:#{data.hp}%] [M:#{data.mana}%]</span></p>")
  end

  defp show_party_member(character, %Companion{} = member, color) do
    data = %{
      name: String.ljust(member.name, 26),
      hp: trunc(member.hp * 100) |> to_string |> String.rjust(3),
      mana: trunc(member.mana * 100) |> to_string |> String.rjust(3)
    }

    Mobile.send_scroll(character, "<p><span class='#{color}'>#{data.name}[H:#{data.hp}%] [M:#{data.mana}%]</span></p>")
  end
end
