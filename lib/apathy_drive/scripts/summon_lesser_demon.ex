defmodule ApathyDrive.Scripts.SummonLesserDemon do
  alias ApathyDrive.{Character, Companion, Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref) do
    monster = Repo.get!(Monster, 1121)

    Room.update_mobile(room, mobile_ref, fn mobile ->
      failure = Enum.random([:return, :attack, :roam])

      %Room{} =
        cond do
          :random.uniform(100) <= mobile.willpower ->
            IO.puts("summon successful")

            room =
              if companion = Character.companion(mobile, room) do
                Companion.dismiss(companion, room)
              else
                room
              end

            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: 5,
                spawned_at: nil,
                zone_spawned_at: nil,
                character_id: mobile.id
              }
              |> Monster.from_room_monster()

            Mobile.send_scroll(
              mobile,
              "<p>The #{Mobile.colored_name(monster)} was successfully controlled.</p>"
            )

            room
            |> Room.mobile_entered(monster, "")
            |> Companion.convert_for_character(monster, mobile)

          failure == :return ->
            IO.puts("summon returned")

            Mobile.send_scroll(
              mobile,
              "<p>The #{Mobile.colored_name(monster)} is not controlled. Annoyed, he returns to his plane.</p>"
            )

            room

          failure == :attack ->
            IO.puts("summon attacked")

            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: 5,
                spawned_at: nil,
                zone_spawned_at: nil,
                decay: true
              }
              |> Monster.from_room_monster()

            Mobile.send_scroll(
              mobile,
              "<p>The #{Mobile.colored_name(monster)} is not controlled. He angrily attacks you!</p>"
            )

            Room.mobile_entered(room, monster, "")

          failure == :roam ->
            IO.puts("summon roamed")

            monster =
              %RoomMonster{
                room_id: room.id,
                monster_id: monster.id,
                level: 5,
                spawned_at: nil,
                zone_spawned_at: nil,
                decay: true
              }
              |> Monster.from_room_monster()
              |> Map.put(:lawful, true)

            Mobile.send_scroll(
              mobile,
              "<p>The #{Mobile.colored_name(monster)} is not controlled, and he goes off in search of bigger and better things.</p>"
            )

            room = Room.mobile_entered(room, monster, "")

            ApathyDrive.AI.move(monster, room, true) || room
        end
    end)
  end
end
