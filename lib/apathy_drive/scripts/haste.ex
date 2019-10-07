defmodule ApathyDrive.Scripts.Haste do
  alias ApathyDrive.{Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      effect = %{
        "Speed" => 0.85
      }

      Systems.Effect.add(mobile, effect, :timer.minutes(1))
    end)
  end
end
