defmodule ApathyDrive.Scripts.SummonAngel do
  alias ApathyDrive.{Character, Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    monster = Repo.get!(Monster, 1127)

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      spellcasting = Mobile.spellcasting_at_level(mobile, mobile.level)

      %Room{} =
        if :rand.uniform(100) < spellcasting do
          owner_id = Map.get(mobile, :room_monster_id) || Map.get(mobile, :id)

          monster =
            %RoomMonster{
              room_id: room.id,
              monster_id: monster.id,
              level: 10,
              spawned_at: nil,
              zone_spawned_at: nil,
              delete_at: Timex.shift(DateTime.utc_now(), minutes: 24),
              owner_id: owner_id
            }
            |> Monster.from_room_monster()
            |> Map.put(:follow, true)

          Mobile.send_scroll(
            mobile,
            "<p><span class='blue'>You summon a #{monster.name}!</span></p>"
          )

          room =
            Room.mobile_entered(
              room,
              monster,
              "A #{Mobile.colored_name(monster)} materializes!"
            )

          Mobile.send_scroll(
            mobile,
            "<p>#{Mobile.colored_name(monster)} begins to follow you.</span></p>"
          )

          room
        else
          room
        end
    end)
  end
end
