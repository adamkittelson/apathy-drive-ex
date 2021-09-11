defmodule ApathyDrive.Scripts.BindLesserDemon do
  alias ApathyDrive.{Ability, Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, _) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      owner_id = mobile.id

      demon =
        room.mobiles
        |> Map.values()
        |> Enum.find(&(&1.id == 1121 and &1.owner_id == owner_id))

      if demon do
        bind_lesser_demon(room, mobile, demon)
      else
        Mobile.send_scroll(
          mobile,
          "<p><span style='dark-cyan'>A demon that was summoned and controlled by you must be present.</span></p>"
        )

        room
      end
    end)
  end

  def bind_lesser_demon(room, mobile, demon) do
    ability = ApathyDrive.Skills.BindLesserDemon.ability(mobile)

    control_chance = ability.traits["ControlChance"]
    defense = ability.traits["Defense"]
    replenishment = ability.traits["Replenishment"]
    duration = ability.traits["Duration"]

    if :rand.uniform(100) < control_chance do
      effects =
        %{
          "StatusMessage" => "A #{demon.name} is bound to your soul.",
          "Defense" => defense,
          "Replenishment" => replenishment,
          # "Grant" => abilities,
          "RemoveMessage" => "The bound #{Mobile.colored_name(demon)} returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 1
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.seconds(duration))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)}.</p>"
      )

      mobile =
        mobile
        |> Systems.Effect.add(effects, :timer.seconds(duration))

      RoomMonster
      |> Repo.get(demon.room_monster_id)
      |> Repo.delete!()

      room
      |> update_in([:mobiles], &Map.delete(&1, demon.ref))
      |> put_in([:mobiles, mobile.ref], mobile)
    else
      room_monster =
        RoomMonster
        |> Repo.get(demon.room_monster_id)
        |> Ecto.Changeset.change(%{
          owner_id: nil,
          delete_at: Timex.shift(DateTime.utc_now(), minutes: 1)
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
