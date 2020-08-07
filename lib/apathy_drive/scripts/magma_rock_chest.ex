defmodule ApathyDrive.Scripts.MagmaRockChest do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      Mobile.send_scroll(mobile, "<p>You open the magma rock chest, and find...</p>")

      room
      |> give_coins_up_to(mobile_ref, %{platinum: 2500, gold: 10000, silver: 25000})
      |> random_item_4102(mobile_ref)
      |> random_item_4102(mobile_ref)
      |> random_item_4102(mobile_ref)
      |> random_item_4102(mobile_ref)
      |> random_item_4102(mobile_ref)
    end)
  end
end
