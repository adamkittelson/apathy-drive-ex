defmodule ApathyDrive.Scripts.Flurry do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.ExtraRound.execute(mobile_ref, target_ref)
    |> Scripts.ExtraRound.execute(mobile_ref, target_ref)
  end
end
