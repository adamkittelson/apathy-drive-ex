defmodule ApathyDrive.Scripts.IncreaseAc do
  alias ApathyDrive.{Ability, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      effect = %{
        "AC" => Ability.ac_for_mitigation_at_level(5),
        "MR" => Ability.ac_for_mitigation_at_level(5)
      }

      Systems.Effect.add(mobile, effect, :timer.minutes(1))
    end)
  end
end
