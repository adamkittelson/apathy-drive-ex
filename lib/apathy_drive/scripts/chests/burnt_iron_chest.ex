defmodule ApathyDrive.Scripts.BurntIronChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the great burnt iron chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{runic: 5, platinum: 100, gold: 10000})
      |> random_item_9387(mobile_ref)
      |> random_item_9387(mobile_ref)
      |> random_item_9387(mobile_ref)
      |> random_item_9385(mobile_ref)
      |> random_item_9385(mobile_ref)
      |> random_item_9385(mobile_ref)
      |> random_item_9387(mobile_ref)
      |> random_item_9387(mobile_ref)
    end)
  end
end
