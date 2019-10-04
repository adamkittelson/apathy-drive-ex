defmodule ApathyDrive.Scripts.GreaterUnworldly do
  alias ApathyDrive.{Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    mobile = room.mobiles[mobile_ref]
    target = room.mobiles[target_ref]

    if mobile && target do
      monster = Repo.get!(Monster, 307)

      owner_id = Map.get(mobile, :room_monster_id) || Map.get(mobile, :id)

      monster =
        %RoomMonster{
          room_id: room.id,
          monster_id: monster.id,
          level: 25,
          spawned_at: nil,
          zone_spawned_at: nil,
          delete_at: Timex.shift(DateTime.utc_now(), minutes: 1),
          owner_id: owner_id
        }
        |> Monster.from_room_monster()

      room = Room.mobile_entered(room, monster, "")

      ApathyDrive.Aggression.attack(room, room.mobiles[monster.ref], target)
    else
      room
    end
  end
end
