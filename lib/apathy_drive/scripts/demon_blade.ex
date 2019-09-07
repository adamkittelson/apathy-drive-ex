defmodule ApathyDrive.Scripts.DemonBlade do
  alias ApathyDrive.{Ability, Character, Companion, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # lesser demon
    1121
  ]

  def execute(%Room{script_args: item} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      case Character.companion(mobile, room) do
        %Companion{monster_id: id} = demon when id in @demon_ids ->
          bind_demon(room, mobile, demon, item)

        _ ->
          Mobile.send_scroll(
            mobile,
            "<p><span style='dark-cyan'>You must have a demon summoned and controlled by you.</span></p>"
          )

          room
      end
    end)
  end

  def bind_demon(room, mobile, demon, item) do
    if :random.uniform(100) < mobile.willpower do
      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)}.</p>"
      )

      RoomMonster
      |> Repo.get(demon.room_monster_id)
      |> Repo.delete!()

      room =
        room
        |> update_in([:mobiles], &Map.delete(&1, demon.ref))
        |> put_in([:mobiles, mobile.ref], mobile)

      case demon.monster_id do
        1121 ->
          # lesser
          ability = Ability.find(3132)
          Ability.execute(room, mobile.ref, ability, item)
      end
    else
      room_monster =
        RoomMonster
        |> Repo.get(demon.room_monster_id)
        |> Ecto.Changeset.change(%{
          character_id: nil,
          decay: true
        })
        |> Repo.update!()

      room = update_in(room, [:mobiles], &Map.delete(&1, demon.ref))

      monster =
        room_monster
        |> Monster.from_room_monster()

      Mobile.send_scroll(
        mobile,
        "<p>The #{Mobile.colored_name(monster)} resists your attempt to bind it, and attacks!</p>"
      )

      Room.mobile_entered(room, monster, "")
    end
  end
end
