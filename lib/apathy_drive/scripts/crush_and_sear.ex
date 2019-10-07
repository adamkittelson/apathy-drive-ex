defmodule ApathyDrive.Scripts.CrushAndSear do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.ImpactCrit.execute(mobile_ref, target_ref)
    |> Scripts.FireECrit.execute(mobile_ref, target_ref)
  end
end
