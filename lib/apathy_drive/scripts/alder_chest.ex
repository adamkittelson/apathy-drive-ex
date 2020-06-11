defmodule ApathyDrive.Scripts.AlderChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the alder chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 5, gold: 750, silver: 2500})
      |> random_item_2921(mobile_ref)
      |> random_item_2921(mobile_ref)
    end)
  end
end
