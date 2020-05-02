defmodule ApathyDrive.Scripts.IceChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the icy chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 500, gold: 7000, silver: 10000})
      |> random_item_2944(mobile_ref)
      |> random_item_2944(mobile_ref)
      |> random_item_2944(mobile_ref)
      |> random_item_2944(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_2944(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
    end)
  end
end
