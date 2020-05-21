defmodule ApathyDrive.Commands.Party do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Party}

  def keywords, do: ["party", "par"]

  # party leader
  def execute(
        %Room{} = room,
        %Character{ref: ref, leader: ref, invitees: _invitees} = character,
        _arguments
      ) do
    show_party_members(character, room)
    room
  end

  # party follower
  def execute(%Room{} = room, %Character{} = character, _arguments) do
    leader = Party.leader(room, character)

    Mobile.send_scroll(
      character,
      "<p><span class='cyan'>You are following #{leader.name}.</span></p>"
    )

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

  defp show_invitee(_character, nil), do: :noop

  defp show_invitee(character, invitee) do
    data = %{
      name: String.pad_trailing(invitee.name, 13)
    }

    Mobile.send_scroll(character, "<p>#{data.name}[Invited]</p>")
  end

  defp show_party_member(character, %Character{} = member, color) do
    data = %{
      name: String.pad_trailing(member.name, 13),
      hp: trunc(member.hp * 100) |> to_string |> String.pad_leading(3),
      mana: trunc(member.mana * 100) |> to_string |> String.pad_leading(3)
    }

    Mobile.send_scroll(
      character,
      "<p><span class='#{color}'>#{data.name}[H:#{data.hp}%] [M:#{data.mana}%]</span></p>"
    )
  end
end
