defmodule ApathyDrive.Scripts.GreaterPower do
  alias ApathyDrive.{Room, Scripts}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    room
    |> Scripts.RaiseWeapon.execute(mobile_ref, target_ref)
    |> Scripts.IncreaseAc.execute(mobile_ref, target_ref)
  end
end
