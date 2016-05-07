defmodule ApathyDrive.Commands.Attack do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["a", "attack", "k", "kill"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
  end

  def execute(mobile, arguments) when is_pid(mobile) do
    target =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    Mobile.attack(mobile, target)
  end

  def execute(%Mobile{room_id: room_id}, query) do
    RoomServer.attack({:global, "room_#{room_id}"}, self(), query)
  end

  def execute(%Room{} = room, mobile, query) do
    target = Room.find_mobile_in_room(room, mobile, query)
    attack(mobile, target && target.pid)
  end

  def attack(mobile, nil) when is_pid(mobile) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
  end

  def attack(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Attack yourself?</p>")
  end

  def attack(mobile, target) when is_pid(mobile) do
    Mobile.attack(mobile, target)
  end

  def attack(%Mobile{} = mobile, target) do
    mobile
    |> Mobile.set_attack_target(target)
    |> Mobile.initiate_combat
    |> Mobile.send_scroll("<p><span class='dark-yellow'>*Combat Engaged*</span></p>")
  end

end
