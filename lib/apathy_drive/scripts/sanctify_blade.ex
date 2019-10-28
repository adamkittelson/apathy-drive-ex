defmodule ApathyDrive.Scripts.SanctifyBlade do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, item) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      effect = %{
        "WeaponDamage" => [
          %{kind: "magical", min: 1, max: 1, damage_type: "Holy", damage_type_id: 9}
        ],
        "RemoveMessage" => "A sanctify blade spell wears off on your #{item.name}.",
        "stack_key" => "sanctify blade",
        "stack_count" => 1
      }

      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You sanctify your #{item.name} and bless it with holy power!</span></p>"
      )

      Room.send_scroll(
        room,
        "<p><span class='blue'>#{character.name} sanctifies their #{item.name} and blesses it with holy power!</span></p>",
        [character]
      )

      item = Systems.Effect.add(item, effect, :timer.minutes(12))

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
