defmodule ApathyDrive.Scripts.VampiricBlade do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, item) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      effect = %{
        "WeaponDamage" => [
          %{kind: "drain", min: 1, max: 1, damage_type: "Unaspected", damage_type_id: 3}
        ],
        "RemoveMessage" => "A vampiric blade spell fades from your #{item.name}.",
        "stack_key" => "lifesteal blade",
        "stack_count" => 1
      }

      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You cast vampiric weapon at #{item.name}!</span></p>"
      )

      Room.send_scroll(
        room,
        "<p><span class='blue'>#{character.name} casts vampiric weapon at #{item.name}!</span></p>",
        [character]
      )

      item = Systems.Effect.add(item, effect, :timer.minutes(24))

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
