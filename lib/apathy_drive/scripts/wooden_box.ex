defmodule ApathyDrive.Scripts.WoodenBox do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      Mobile.send_scroll(mobile, "<p>You open the wooden box, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{gold: 30, silver: 300})
      |> random_item_871(mobile_ref)
      |> random_item_871(mobile_ref)
      |> random_item_874(mobile_ref)
    end)
  end
end
