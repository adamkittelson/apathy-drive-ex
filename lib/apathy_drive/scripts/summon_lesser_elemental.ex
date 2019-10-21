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
        |> Map.put(:follow, true)

      Mobile.send_scroll(mobile, "<p><span class='blue'>You summon a #{monster.name}!</span></p>")

      room = Room.mobile_entered(room, monster, "A #{Mobile.colored_name(monster)} materializes!")

      Mobile.send_scroll(
        mobile,
        "<p>#{Mobile.colored_name(monster)} begins to follow you.</span></p>"
      )

      room
    end)
  end
end
