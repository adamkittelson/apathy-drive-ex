defmodule ApathyDrive.Scripts.PoisonRune do
  alias ApathyDrive.{Ability, Character, Item, ItemInstance, Mobile, Repo, Room}

  @poison_rune_item_id 4

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
              "<p><span class='green'>#{Mobile.colored_name(mobile)}</span> is poisoned by a rune!</p>",
              [mobile]
            )

            ability = %Ability{
              kind: "curse",
              name: "poison rune",
              energy: 0,
              mana: 0,
              duration: 60,
              traits: %{
                "Poison" => div(Mobile.max_hp_at_level(mobile, mobile.level), 2),
                "StatusMessage" => "You feel ill!",
                "StackKey" => :poison_rune,
                "StackCount" => :infinity
              }
            }

            room = Ability.execute(room, mobile.ref, ability, [mobile.ref])

            room
        end)

      _item, room ->
        room
    end)
  end
end
