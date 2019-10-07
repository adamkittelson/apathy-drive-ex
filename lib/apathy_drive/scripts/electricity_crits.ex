defmodule ApathyDrive.Scripts.ElectricityCrits do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
    |> Scripts.ElectricityCrit.execute(mobile_ref, target_ref)
  end
end
