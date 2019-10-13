defmodule ApathyDrive.Scripts.LesserElementalBlade do
  alias ApathyDrive.{Mobile, Room}

  @damage %{min: 1, max: 1}

  def execute(%Room{} = room, mobile_ref, item) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      lore = character.lore

      damage =
        lore.damage_types
        |> Enum.map(fn damage_type ->
          Map.merge(damage_type, %{
            min: @damage.min,
            max: @damage.max
          })
        end)

      effect = %{
        "WeaponDamage" => damage,
        "RemoveMessage" => "A #{lore.name} damage spell fades from your #{item.name}.",
        "stack_key" => "elemental blade",
        "stack_count" => 3
      }

      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You cast lesser #{lore.name} blade at #{item.name}!</span></p>"
      )

      Room.send_scroll(
        room,
        "<p><span class='blue'>#{character.name} casts lesser #{lore.name} blade at #{item.name}!</span></p>",
        [character]
      )

      item = Systems.Effect.add(item, effect, :timer.minutes(10))

      equipment_location =
        Enum.find_index(
          character.equipment,
          &(&1.instance_id == item.instance_id)
        )

      inventory_location =
        Enum.find_index(
          character.inventory,
          &(&1.instance_id == item.instance_id)
        )

      if equipment_location do
        update_in(character.equipment, &List.replace_at(&1, equipment_location, item))
      else
        update_in(character.inventory, &List.replace_at(&1, inventory_location, item))
      end
    end)
  end
end
