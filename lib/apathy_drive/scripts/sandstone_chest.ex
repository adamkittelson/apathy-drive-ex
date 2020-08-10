defmodule ApathyDrive.Scripts.SandstoneChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the sandstone chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 500, gold: 1000, silver: 15000})
      |> random_item_4151(mobile_ref)
      |> random_item_4151(mobile_ref)
    end)
  end
end
