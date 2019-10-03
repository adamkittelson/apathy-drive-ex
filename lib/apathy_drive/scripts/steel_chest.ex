defmodule ApathyDrive.Scripts.SteelChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the steel chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 5, gold: 500, silver: 1000})
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
    end)
  end
end
