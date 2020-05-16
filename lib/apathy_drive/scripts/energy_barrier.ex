defmodule ApathyDrive.Scripts.EnergyBarrier do
  alias ApathyDrive.Room

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      bubble =
        15..40
        |> Enum.random()

      effect = %{
        "Bubble%" => bubble,
        "RemoveMessage" => "The energy barrier around you fades away."
      }

      Systems.Effect.add(mobile, effect, :timer.minutes(2))
    end)
  end
end
