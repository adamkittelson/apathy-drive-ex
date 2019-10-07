defmodule ApathyDrive.Scripts.SpontaneousCombust do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.FireECrit.execute(mobile_ref, target_ref)
    |> Scripts.ImpactCrit.execute(mobile_ref, target_ref)
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
  end
end
