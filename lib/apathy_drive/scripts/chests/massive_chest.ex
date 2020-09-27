defmodule ApathyDrive.Scripts.MassiveChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the massive chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 20, gold: 300, silver: 2000, copper: 15000})
      |> random_item_4106(mobile_ref)
      |> random_item_4106(mobile_ref)
      |> random_item_4106(mobile_ref)
      |> random_item_4106(mobile_ref)
      |> random_item_4106(mobile_ref)
    end)
  end
end
