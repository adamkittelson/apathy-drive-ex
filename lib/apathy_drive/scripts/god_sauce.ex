# fire_E_crit
# fire_E_crit
# impact_crit
# impact_crit
# electricity_crit
# electricity_crit
defmodule ApathyDrive.Scripts.GodSauce do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.FireECrit.execute(mobile_ref, target_ref)
    |> Scripts.FireECrit.execute(mobile_ref, target_ref)
    |> Scripts.ImpactCrit.execute(mobile_ref, target_ref)
    |> Scripts.ImpactCrit.execute(mobile_ref, target_ref)
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
  end
end
