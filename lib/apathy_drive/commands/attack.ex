defmodule ApathyDrive.Commands.Attack do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["a", "attack", "k", "kill"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    target = Room.find_mobile_in_room(room, mobile, query)

    Room.update_mobile(room, mobile.ref, fn %Mobile{} = attacker ->
      attack(attacker, target)
    end)
  end

  def attack(%Mobile{} = mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
  end

  def attack(%Mobile{ref: attacker_ref} =  mobile, %Mobile{ref: target_ref}) when attacker_ref == target_ref do
    Mobile.send_scroll(mobile, "<p>Attack yourself?</p>")
  end

  def attack(%Mobile{} = mobile, %Mobile{ref: target_ref}) do
    mobile
    |> Mobile.set_attack_target(target_ref)
    |> Mobile.initiate_combat
    |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
  end

end
