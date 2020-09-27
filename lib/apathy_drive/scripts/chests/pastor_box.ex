defmodule ApathyDrive.Scripts.PastorBox do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn
      room, mobile ->
        Mobile.send_scroll(mobile, "<p>You open the silver casket, and find...</p>")

        room = give_coins_up_to(room, mobile_ref, %{gold: 30, silver: 300})

        if Enum.find(mobile.classes, &(&1.class_id == 5)) do
          # pristine scroll, blessed scroll, white scroll
          scroll = Enum.random([1878, 1877, 2011])

          room
          |> give_item(mobile_ref, scroll)
        else
          room
        end
        |> random_item_9645(mobile_ref)
    end)
  end
end
