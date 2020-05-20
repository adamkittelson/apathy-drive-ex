defmodule ApathyDrive.Scripts.ExplodingRune do
  alias ApathyDrive.{Ability, Character, Item, ItemInstance, Mobile, Repo, Room}

  @exploding_rune_item_id 5

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      %ItemInstance{
        item_id: @exploding_rune_item_id,
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

  def activate(room, mobile) do
    Enum.reduce(room.items, room, fn
      %Item{id: @exploding_rune_item_id, dropped_for_character_id: id}, room ->
        Room.update_mobile(room, mobile.ref, fn
          room, %Character{id: ^id} ->
            room

          room, %{} = mobile ->
            # percentage of health for characters,
            # raw damage for monsters
            {min, max} =
              case mobile do
                %Character{} ->
                  max_hp = Mobile.max_hp_at_level(mobile, mobile.level)

                  {trunc(max_hp * 0.25), trunc(max_hp * 0.75)}

                _ ->
                  {25, 75}
              end

            ability = %Ability{
              kind: "attack",
              name: "exploding rune",
              energy: 0,
              mana: 0,
              target_message: "You are hit by an exploding rune for {{amount}} damage!",
              spectator_message: "{{Target}} is hit by an exploding rune for {{amount}} damage!",
              traits: %{
                "Damage" => [
                  %{
                    kind: "magical",
                    min: trunc(min * 0.75),
                    max: trunc(max * 0.75),
                    damage_type: "Fire",
                    damage_type_id: 5
                  },
                  %{
                    kind: "magical",
                    min: trunc(min * 0.25),
                    max: trunc(max * 0.25),
                    damage_type: "Impact",
                    damage_type_id: 11
                  }
                ]
              }
            }

            Ability.execute(room, mobile.ref, ability, [mobile.ref])
        end)

      _item, room ->
        room
    end)
  end
end
