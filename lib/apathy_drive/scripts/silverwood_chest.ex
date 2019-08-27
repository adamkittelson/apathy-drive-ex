defmodule ApathyDrive.Scripts.SilverwoodChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      Mobile.send_scroll(mobile, "<p>You open the silverwood chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{runic: 2, platinum: 25, gold: 1000, silver: 2000})
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_895(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
      |> random_item_898(mobile_ref)
    end)
  end
end
