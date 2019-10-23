defmodule ApathyDrive.Scripts.BindDemon do
  alias ApathyDrive.{Ability, Character, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # lesser demon
    1121
  ]

  def execute(%Room{} = room, mobile_ref, _) do
    room = update_in(room.mobiles[mobile_ref], &Character.alter_evil_points(&1, 1))

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      owner_id = mobile.id

      demon =
        room.mobiles
        |> Map.values()
        |> Enum.find(&(&1.id in @demon_ids and &1.owner_id == owner_id))

      case demon do
        %Monster{id: 1121} = demon ->
          bind_lesser_demon(room, mobile, demon)

        _ ->
          Mobile.send_scroll(
            mobile,
            "<p><span style='dark-cyan'>A demon that was summoned and controlled by you must be present.</span></p>"
          )

          room
      end
    end)
  end

  def bind_lesser_demon(room, mobile, demon) do
    abilities =
      demon.abilities
      |> Map.values()
      |> Enum.reject(&(&1.kind == "auto attack"))
      |> Enum.map(& &1.name)

    spellcasting = Mobile.spellcasting_at_level(mobile, mobile.level) + 45

    if :random.uniform(100) < spellcasting do
      effects =
        %{
          "StatusMessage" => "A #{demon.name} is bound to your skin.",
          "Strength" => 5,
          "Willpower" => 5,
          "Agility" => 5,
          "AC%" => 5,
          "MR%" => 5,
          "Heal" => %{"max" => 2, "min" => 2},
          "Encumbrance" => 5,
          "Grant" => abilities,
          "RemoveMessage" =>
            "The #{Mobile.colored_name(demon)} bound to your skin returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 3
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.minutes(80))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your skin.</p>"
      )

      known_abilities =
        mobile.abilities
        |> Map.values()
        |> List.flatten()
        |> Enum.map(& &1.name)

      abilities = abilities -- known_abilities

      if Enum.any?(abilities) do
        Mobile.send_scroll(
          mobile,
          "<p>You've gain the ability to cast <span class='dark-cyan'>#{
            ApathyDrive.Commands.Inventory.to_sentence(abilities)
          }</span>!</p>"
        )
      end

      mobile =
        mobile
        |> Systems.Effect.add(effects, :timer.minutes(80))
        |> Character.load_abilities()

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
