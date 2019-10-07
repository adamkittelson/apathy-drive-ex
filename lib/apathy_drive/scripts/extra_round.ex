defmodule ApathyDrive.Scripts.ExtraRound do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    mobile = room.mobiles[mobile_ref]
    target = room.mobiles[target_ref]

    if mobile && target do
      mobile = Map.put(mobile, :energy, mobile.max_energy)
      room = put_in(room.mobiles[mobile_ref].energy, mobile.max_energy)

      ApathyDrive.AI.auto_attack(mobile, room, target_ref) || room
    else
      room
    end
  end
end
