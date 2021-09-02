defmodule ApathyDrive.Scripts.SummonLesserDemon do
  alias ApathyDrive.{Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    monster = Repo.get!(Monster, 1121)

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      ability = ApathyDrive.Skills.SummonLesserDemon.ability(mobile)

      control_chance = control_chance(room, mobile, ability.traits["ControlChance"])

      IO.puts("control chance: #{control_chance}")
      duration = ability.traits["Duration"]

      failure = Enum.random([:return, :attack, :roam])

      %Room{} =
        cond do
          :rand.uniform(100) <= control_chance ->
            owner_id = Map.get(mobile, :room_monster_id) || Map.get(mobile, :id)

            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: mobile.level,
                spawned_at: nil,
                zone_spawned_at: nil,
                delete_at: Timex.shift(DateTime.utc_now(), seconds: duration),
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

          failure == :return ->
            Mobile.send_scroll(
              mobile,
              "<p>The #{Mobile.colored_name(monster)} is not controlled. Annoyed, he returns to his plane.</p>"
            )

            room

          failure == :attack ->
            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: mobile.level,
                spawned_at: nil,
                zone_spawned_at: nil,
                delete_at: Timex.shift(DateTime.utc_now(), seconds: duration)
              }
              |> Monster.from_room_monster()

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
              "<p>The #{Mobile.colored_name(monster)} is not controlled. He angrily attacks you!</p>"
            )

            room

          failure == :roam ->
            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: mobile.level,
                spawned_at: nil,
                zone_spawned_at: nil,
                delete_at: Timex.shift(DateTime.utc_now(), seconds: duration)
              }
              |> Monster.from_room_monster()
              |> Map.put(:lawful, true)

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
              "<p>The #{Mobile.colored_name(monster)} is not controlled, and he goes off in search of bigger and better things.</p>"
            )

            ApathyDrive.AI.move(monster, room, true) || room
        end
    end)
  end

  defp control_chance(room, mobile, chance) do
    controlled_demons =
      room.mobiles
      |> Map.values()
      |> Enum.filter(&(IO.inspect(&1[:owner_id]) == mobile.id))
      |> length()

    chance - controlled_demons * 15
  end
end
