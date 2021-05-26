defmodule ApathyDrive.Scripts.RaiseWeapon do
  alias ApathyDrive.{Character, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn
      _room, %Character{} = mobile ->
        weapon = Character.weapon(mobile)

        if weapon do
          effect = %{
            "WeaponDamage" => [
              %{
                damage_type: "Magical",
                min: 1,
                max: 1
              }
            ],
            "RemoveMessage" => "The shimmer on your weapon fades."
          }

          weapon = Systems.Effect.add(weapon, effect, :timer.minutes(1))

          location =
            Enum.find_index(
              mobile.equipment,
              &(&1.instance_id == weapon.instance_id)
            )

          update_in(mobile.equipment, &List.replace_at(&1, location, weapon))
        else
          mobile
        end

      room, _mobile ->
        room
    end)
  end
end
