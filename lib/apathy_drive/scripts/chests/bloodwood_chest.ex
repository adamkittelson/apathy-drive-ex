defmodule ApathyDrive.Scripts.BloodwoodChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the great bloodwood chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{runic: 10, platinum: 100, gold: 50000})
      |> random_item_9387(mobile_ref)
      |> random_item_9387(mobile_ref)
      |> random_item_9387(mobile_ref)
      |> random_item_9467(mobile_ref)
      |> random_item_9468(mobile_ref)
      |> random_item_9469(mobile_ref)
      |> random_item_9470(mobile_ref)
    end)
  end
end
