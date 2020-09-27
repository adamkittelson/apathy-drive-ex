defmodule ApathyDrive.Scripts.OakChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the oak chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{gold: 60, silver: 600})
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_874(mobile_ref)
      |> random_item_874(mobile_ref)
      |> random_item_874(mobile_ref)
    end)
  end
end
