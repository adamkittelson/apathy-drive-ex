defmodule ApathyDrive.Scripts.HealingRune do
  alias ApathyDrive.{ItemInstance, Repo, Room}

  @healing_rune_item_id 10

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      %ItemInstance{
        item_id: @healing_rune_item_id,
        room_id: room.id,
        character_id: nil,
        dropped_for_character_id: mobile.id,
        equipped: false,
        hidden: false,
        delete_at: Timex.shift(DateTime.utc_now(), minutes: 12)
      }
      |> Repo.insert!()

      Room.load_items(room)
    end)
  end

  def item_id, do: @healing_rune_item_id
end
