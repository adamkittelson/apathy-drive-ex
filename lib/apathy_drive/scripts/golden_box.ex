defmodule ApathyDrive.Scripts.GoldenBox do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      Mobile.send_scroll(mobile, "<p>You open the golden box, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{gold: 120, silver: 1200})
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
    end)
  end
end
