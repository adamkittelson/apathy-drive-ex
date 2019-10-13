defmodule ApathyDrive.Scripts.SummonLesserElemental do
  alias ApathyDrive.{Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    monster = Repo.get!(Monster, 1122)

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      owner_id = Map.get(mobile, :room_monster_id) || Map.get(mobile, :id)

      monster =
        %RoomMonster{
          room_id: room.id,
          monster_id: monster.id,
          lore: mobile.lore.name,
          level: 5,
          spawned_at: nil,
          zone_spawned_at: nil,
          delete_at: Timex.shift(DateTime.utc_now(), minutes: 5),
          owner_id: owner_id
        }
        |> Monster.from_room_monster()

      Mobile.send_scroll(mobile, "<p><span class='blue'>You summon a #{monster.name}!</span></p>")

      Room.send_scroll(room, "<p><span class='blue'>You summon a #{monster.name}!</span></p>", [
        mobile
      ])

      Room.mobile_entered(room, monster, "")
    end)
  end
end
