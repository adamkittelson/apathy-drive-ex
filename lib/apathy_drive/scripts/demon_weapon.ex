defmodule ApathyDrive.Scripts.DemonWeapon do
  alias ApathyDrive.{Item, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # lesser demon
    1121,
    # demon
    1126
  ]

  def execute(%Room{} = room, mobile_ref, item) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      owner_id = mobile.id

      demon =
        room.mobiles
        |> Map.values()
        |> Enum.find(&(&1.id in @demon_ids and &1.owner_id == owner_id))

      case demon do
        %Monster{id: 1121} = demon ->
          lesser_demon_weapon(room, mobile, demon, item)

        %Monster{id: 1126} = demon ->
          demon_weapon(room, mobile, demon, item)

        _ ->
          Mobile.send_scroll(
            mobile,
            "<p><span style='dark-cyan'>A demon that was summoned and controlled by you must be present.</span></p>"
          )

          room
      end
    end)
  end

  def lesser_demon_weapon(room, mobile, demon, item) do
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["willpower"]}) + 45

    if :rand.uniform(100) < spellcasting do
      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your #{
          Item.colored_name(item)
        }.</p>"
      )

      Room.send_scroll(
        room,
        "<p>#{Mobile.colored_name(mobile)} binds a #{Mobile.colored_name(demon)} to their #{
          Item.colored_name(item)
        }.</p>",
        [mobile]
      )

      RoomMonster
      |> Repo.get(demon.room_monster_id)
      |> Repo.delete!()

      room =
        room
        |> update_in([:mobiles], &Map.delete(&1, demon.ref))
        |> put_in([:mobiles, mobile.ref], mobile)

      effect = %{
        "WeaponDamage" => [
          %{kind: "magical", min: 1, max: 1, damage_type: "Disruption", damage_type_id: 13},
          %{kind: "magical", min: 1, max: 1, damage_type: "Fire", damage_type_id: 5}
        ],
        "RemoveMessage" => "The #{demon.name} bound to your #{item.name} returns to its plane.",
        "stack_key" => "demon-weapon",
        "stack_count" => 1
      }

      item = Systems.Effect.add(item, effect, :timer.minutes(30))

      effect = %{
        "StatusMessage" => "A #{demon.name} is bound to your #{item.name}."
      }

      room =
        update_in(room.mobiles[mobile.ref], &Systems.Effect.add(&1, effect, :timer.minutes(30)))

      equipment_location =
        Enum.find_index(
          mobile.equipment,
          &(&1.instance_id == item.instance_id)
        )

      inventory_location =
        Enum.find_index(
          mobile.inventory,
          &(&1.instance_id == item.instance_id)
        )

      if equipment_location do
        update_in(
          room.mobiles[mobile.ref].equipment,
          &List.replace_at(&1, equipment_location, item)
        )
      else
        update_in(
          room.mobiles[mobile.ref].inventory,
          &List.replace_at(&1, inventory_location, item)
        )
      end
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

  def demon_weapon(room, mobile, demon, item) do
    spellcasting =
      Mobile.spellcasting_at_level(mobile, mobile.level, %{attributes: ["willpower"]})

    if :rand.uniform(100) < spellcasting do
      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)} to your #{
          Item.colored_name(item)
        }.</p>"
      )

      Room.send_scroll(
        room,
        "<p>#{Mobile.colored_name(mobile)} binds a #{Mobile.colored_name(demon)} to their #{
          Item.colored_name(item)
        }.</p>",
        [mobile]
      )

      RoomMonster
      |> Repo.get(demon.room_monster_id)
      |> Repo.delete!()

      room =
        room
        |> update_in([:mobiles], &Map.delete(&1, demon.ref))
        |> put_in([:mobiles, mobile.ref], mobile)

      effect = %{
        "WeaponDamage" => [
          %{kind: "magical", min: 2, max: 2, damage_type: "Disruption", damage_type_id: 13},
          %{kind: "magical", min: 2, max: 2, damage_type: "Fire", damage_type_id: 5},
          %{kind: "magical", min: 2, max: 2, damage_type: "Stress", damage_type_id: 21}
        ],
        "RemoveMessage" => "The #{demon.name} bound to your #{item.name} returns to its plane.",
        "stack_key" => "demon-weapon",
        "stack_count" => 1
      }

      item = Systems.Effect.add(item, effect, :timer.minutes(60))

      effect = %{
        "StatusMessage" => "A #{demon.name} is bound to your #{item.name}."
      }

      room =
        update_in(room.mobiles[mobile.ref], &Systems.Effect.add(&1, effect, :timer.minutes(60)))

      equipment_location =
        Enum.find_index(
          mobile.equipment,
          &(&1.instance_id == item.instance_id)
        )

      inventory_location =
        Enum.find_index(
          mobile.inventory,
          &(&1.instance_id == item.instance_id)
        )

      if equipment_location do
        update_in(
          room.mobiles[mobile.ref].equipment,
          &List.replace_at(&1, equipment_location, item)
        )
      else
        update_in(
          room.mobiles[mobile.ref].inventory,
          &List.replace_at(&1, inventory_location, item)
        )
      end
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
