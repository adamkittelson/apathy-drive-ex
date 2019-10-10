defmodule ApathyDrive.Scripts.IncreaseStr do
  alias ApathyDrive.Room

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      effect = %{
        "Strength" => 5
      }

      Systems.Effect.add(mobile, effect, :timer.minutes(1))
    end)
  end
end
