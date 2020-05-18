defmodule ApathyDrive.Scripts.PoisonRune do
  alias ApathyDrive.{Character, Item, ItemInstance, Mobile, Repo, Room}

  @poison_rune_item_id 3

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      %ItemInstance{
        item_id: @poison_rune_item_id,
        room_id: room.id,
        character_id: nil,
        dropped_for_character_id: mobile.id,
        equipped: false,
        hidden: false,
        delete_at: Timex.shift(DateTime.utc_now(), minutes: 24)
      }
      |> Repo.insert!()

      Room.load_items(room)
    end)
  end

  def activate(room, mobile) do
    Enum.reduce(room.items, room, fn
      %Item{id: @poison_rune_item_id, dropped_for_character_id: id}, room ->
        Room.update_mobile(room, mobile.ref, fn
          room, %Character{id: ^id} ->
            room

          room, %{} = mobile ->
            Mobile.send_scroll(
              mobile,
              "<p><span class='green'>You are poisoned by a rune!</span></p>"
            )

            Room.send_scroll(
              room,
              "<p><span class='green'>#{Mobile.colored_name(mobile)}</span> is poisoned by a rune!</p>"
            )

            Systems.Effect.add(
              mobile,
              %{
                "Poison" => 0.5,
                "StatusMessage" => "You feel ill!",
                "stack_key" => :poison_rune,
                "stack_count" => :infinity
              },
              :timer.seconds(60)
            )
        end)

      _item, room ->
        room
    end)
  end
end
