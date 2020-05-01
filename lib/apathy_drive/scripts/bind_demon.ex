defmodule ApathyDrive.Scripts.BindDemon do
  alias ApathyDrive.{Ability, Character, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # lesser demon
    1121,
    # demon
    1126,
    # greater demon
    1128,
    # demon lord
    1129
  ]

  def execute(%Room{} = room, mobile_ref, _) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      owner_id = mobile.id

      demon =
        room.mobiles
        |> Map.values()
        |> Enum.find(&(&1.id in @demon_ids and &1.owner_id == owner_id))

      case demon do
        %Monster{id: 1121} = demon ->
          bind_lesser_demon(room, mobile, demon)

        %Monster{id: 1126} = demon ->
          bind_demon(room, mobile, demon)

        %Monster{id: 1128} = demon ->
          bind_greater_demon(room, mobile, demon)

        %Monster{id: 1129} = demon ->
          bind_demon_lord(room, mobile, demon)

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
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["intellect"]})

    if :rand.uniform(100) < spellcasting + 15 do
      effects =
        %{
          # "Bubble%" => 10,
          # "BubbleRegen%PerSecond" => 0.5,
          "StatusMessage" => "A #{demon.name} is bound to your skin.",
          "AC%" => 5,
          "MR%" => 10,
          "DarkVision" => 225,
          "RestoreLimbs" => true,
          "Encumbrance" => 10,
          # "Grant" => abilities,
          "RemoveMessage" =>
            "The #{Mobile.colored_name(demon)} bound to your skin returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 1
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.minutes(80))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your skin.</p>"
      )

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

  def bind_demon(room, mobile, demon) do
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["intellect"]})

    if :rand.uniform(100) < spellcasting - 5 do
      effects =
        %{
          # "Bubble%" => 15,
          # "BubbleRegen%PerSecond" => 0.5,
          "StatusMessage" => "A #{demon.name} is bound to your skin.",
          "AC%" => 10,
          "MR%" => 20,
          "DarkVision" => 225,
          "RestoreLimbs" => true,
          "Encumbrance" => 15,
          # "Grant" => abilities,
          "RemoveMessage" =>
            "The #{Mobile.colored_name(demon)} bound to your skin returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 1
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.minutes(95))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your skin.</p>"
      )

      mobile =
        mobile
        |> Systems.Effect.add(effects, :timer.minutes(95))

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

  def bind_greater_demon(room, mobile, demon) do
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["intellect"]})

    if :rand.uniform(100) < spellcasting - 20 do
      effects =
        %{
          # "Bubble%" => 15,
          # "BubbleRegen%PerSecond" => 0.5,
          "StatusMessage" => "A #{demon.name} is bound to your skin.",
          "AC%" => 15,
          "MR%" => 30,
          "DarkVision" => 225,
          "RestoreLimbs" => true,
          "Encumbrance" => 20,
          # "Grant" => abilities,
          "RemoveMessage" =>
            "The #{Mobile.colored_name(demon)} bound to your skin returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 1
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.minutes(95))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your skin.</p>"
      )

      mobile =
        mobile
        |> Systems.Effect.add(effects, :timer.minutes(110))

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

  def bind_demon_lord(room, mobile, demon) do
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["intellect"]})

    if :rand.uniform(100) < spellcasting - 45 do
      effects =
        %{
          # "Bubble%" => 15,
          # "BubbleRegen%PerSecond" => 0.5,
          "StatusMessage" => "A #{demon.name} is bound to your skin.",
          "AC%" => 20,
          "MR%" => 40,
          "DarkVision" => 225,
          "RestoreLimbs" => true,
          "Encumbrance" => 25,
          # "Grant" => abilities,
          "RemoveMessage" =>
            "The #{Mobile.colored_name(demon)} bound to your skin returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 1
        }
        |> Map.put("effect_ref", make_ref())
        |> Ability.process_duration_traits(mobile, mobile, :timer.minutes(95))

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your skin.</p>"
      )

      mobile =
        mobile
        |> Systems.Effect.add(effects, :timer.minutes(120))

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
