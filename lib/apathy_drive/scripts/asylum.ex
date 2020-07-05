defmodule ApathyDrive.Scripts.Asylum do
  alias ApathyDrive.{ItemInstance, Mobile, Repo, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    asylum = 3

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      %ItemInstance{
        item_id: asylum,
        room_id: room.id,
        character_id: nil,
        owner_id: mobile.id,
        equipped: false,
        hidden: false,
        delete_at: Timex.shift(DateTime.utc_now(), minutes: 3)
      }
      |> Repo.insert!()

      Room.load_items(room)
    end)
  end

  def enforce_asylum(room, mobile) do
    asylums = Enum.filter(room.items, &(&1.id == 3))

    if Enum.any?(asylums, &(&1.owner_id != mobile.id)) do
      Mobile.send_scroll(
        mobile,
        "<p>This room is a prison that you cannot leave until the asylum spell fades.</p>"
      )

      Room.send_scroll(
        room,
        "<p><span class='yellow'>#{mobile.name}</span> <span class='dark-green'>tries to leave but is confined by the asylum spell!</span></p>",
        [mobile]
      )
    end
  end
end
