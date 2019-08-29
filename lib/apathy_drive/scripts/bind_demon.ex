defmodule ApathyDrive.Scripts.BindDemon do
  alias ApathyDrive.{Character, Companion, Mobile, Monster, Repo, Room, RoomMonster}

  @demon_ids [
    # lesser demon
    1121
  ]

  def execute(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
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
      abilities = ["disruption bolt", "lesser blood surge"]

      effects =
        %{
          "StatusMessage" => "You are possesed by a #{demon.name}.",
          "Strength" => 1,
          "Agility" => 1,
          "Intellect" => 1,
          "Health" => 1,
          "Charm" => 1,
          "Willpower" => -5,
          "Grant" => abilities,
          "DarkVision" => 10,
          "AC%" => 1,
          "MR%" => 1,
          "Speed" => 0.99,
          "ManaRegen" => 10,
          "MaxMana" => 2,
          "Encumbrance" => 1,
          "HPRegen" => 10,
          "Accuracy" => 1,
          "Dodge" => 1,
          "RemoveMessage" => "The #{Mobile.colored_name(demon)} returns to its plane."
        }
        |> Map.put("effect_ref", make_ref())

      Mobile.send_scroll(
        mobile,
        "<p>You successfully bind the #{Mobile.colored_name(demon)}.</p>"
      )

      Mobile.send_scroll(
        mobile,
        "<p>You've gain the ability to cast <span class='dark-cyan'>#{
          ApathyDrive.Commands.Inventory.to_sentence(abilities)
        }</span>!</p>"
      )

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
