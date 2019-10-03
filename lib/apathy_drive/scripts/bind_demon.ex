defmodule ApathyDrive.Scripts.BindDemon do
  alias ApathyDrive.{Ability, Character, Companion, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # minor blood demon
    1121
  ]

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      case Character.companion(mobile, room) do
        %Companion{monster_id: id} = demon when id in @demon_ids ->
          bind_demon(room, mobile, demon)

        _ ->
          Mobile.send_scroll(
            mobile,
            "<p><span style='dark-cyan'>You must have a demon summoned and controlled by you.</span></p>"
          )

          room
      end
    end)
  end

  def bind_demon(room, mobile, demon) do
    if :random.uniform(100) < mobile.willpower do
      abilities = ["vampiric touch", "blur", "ethereal shield"]

      effects =
        %{
          "StatusMessage" => "You are possesed by a #{demon.name}.",
          "Strength" => 2,
          "Health" => 2,
          "Grant" => abilities,
          "AC%" => 2,
          "MR%" => 2,
          "Encumbrance" => 5,
          "RemoveMessage" => "The #{Mobile.colored_name(demon)} returns to its plane.",
          "stack_key" => "bind-demon",
          "stack_count" => 3
        }
        |> Map.put("effect_ref", make_ref())

      effects =
        Ability.process_duration_trait(
          {"Heal", %{"max" => 2, "min" => 2}},
          effects,
          mobile,
          mobile,
          nil
        )

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)}.</p>"
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
        |> Systems.Effect.add(effects, :timer.minutes(60))
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
