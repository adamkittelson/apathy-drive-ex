defmodule ApathyDrive.Scripts.EnergyBarrier do
  alias ApathyDrive.{Ability, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      ac =
        15..40
        |> Enum.random()
        |> Ability.ac_for_mitigation_at_level(mobile.level)

      effect = %{
        "AC" => ac,
        "MR" => ac,
        "RemoveMessage" => "The energy barrier around you fades away."
      }

      Systems.Effect.add(mobile, effect, :timer.minutes(2))
    end)
  end
end
