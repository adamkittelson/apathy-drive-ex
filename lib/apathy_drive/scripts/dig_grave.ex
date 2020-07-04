defmodule ApathyDrive.Scripts.DigGrave do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      # shovel
      if check_item(mobile, 1415, "Nothing happens.") do
        dig_grave(room, mobile)
      else
        room
      end
    end)
  end

  defp dig_grave(room, mobile) do
    if 1000 <= mobile.energy do
      roll = :rand.uniform(100)

      room =
        Room.update_mobile(room, mobile.ref, fn _room, mobile ->
          update_in(mobile.energy, &(&1 - 1000))
        end)

      Room.update_energy_bar(room, mobile.ref)

      Mobile.send_scroll(
        mobile,
        "<p>You use your shovel and dig up a grave, to find...</p>"
      )

      Room.send_scroll(
        room,
        "<p>#{mobile.name} uses their shovel and dig up a grave, and finds...</p>",
        [mobile]
      )

      cond do
        roll <= 2 ->
          Mobile.send_scroll(
            mobile,
            "<p>Suddenly your shovel breaks in two, becoming useless!</p>"
          )

          Room.send_scroll(
            room,
            "<p>#{mobile.name}'s shovel breaks in two, becoming useless!</p>",
            [mobile]
          )

          # shovel
          take_item(room, mobile.ref, 1415)

        roll <= 30 ->
          random_30(room, mobile.ref)

        roll <= 50 ->
          random_50(room, mobile.ref)

        roll <= 70 ->
          random_70(room, mobile.ref)

        roll <= 90 ->
          random_90(room, mobile.ref)

        roll <= 100 ->
          Mobile.send_scroll(mobile, "<p>Nothing.</p>")

          Room.send_scroll(
            room,
            "<p>Nothing.</p>",
            [mobile]
          )

          room
      end
    else
      Mobile.send_scroll(
        mobile,
        "<p>You begin digging...</p>"
      )

      Map.put(mobile, :casting, {ApathyDrive.Scripts.DigGrave, mobile.ref, 1000})
    end
  end

  def random_30(room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      roll = :rand.uniform(100)

      cond do
        roll <= 80 ->
          Mobile.send_scroll(mobile, "<p>Nothing.</p>")

          Room.send_scroll(
            room,
            "<p>Nothing.</p>",
            [mobile]
          )

          room

        roll <= 81 ->
          # longsword
          give_item(room, mobile_ref, 64)

        roll <= 82 ->
          # trident
          give_item(room, mobile_ref, 452)

        roll <= 83 ->
          # falchion
          give_item(room, mobile_ref, 76)

        roll <= 84 ->
          # battle axe
          give_item(room, mobile_ref, 71)

        roll <= 85 ->
          # halberd
          give_item(room, mobile_ref, 82)

        roll <= 86 ->
          # warhammer
          give_item(room, mobile_ref, 91)

        roll <= 87 ->
          # bladed staff
          give_item(room, mobile_ref, 461)

        roll <= 88 ->
          # broadsword
          give_item(room, mobile_ref, 65)

        roll <= 89 ->
          # greatsword
          give_item(room, mobile_ref, 78)

        roll <= 90 ->
          # bastard sword
          give_item(room, mobile_ref, 80)

        roll <= 91 ->
          # scalemail leggings
          give_item(room, mobile_ref, 25)

        roll <= 92 ->
          # chainmail leggings
          give_item(room, mobile_ref, 24)

        roll <= 93 ->
          # full plate leggings
          give_item(room, mobile_ref, 28)

        roll <= 95 ->
          # greatcloak
          give_item(room, mobile_ref, 56)

        roll <= 96 ->
          # chain coif
          give_item(room, mobile_ref, 34)

        roll <= 97 ->
          # plate boots
          give_item(room, mobile_ref, 51)

        roll <= 98 ->
          # chain gauntlets
          give_item(room, mobile_ref, 42)

        roll <= 99 ->
          # skeleton
          summon(room, 11)

        roll <= 100 ->
          random_90(room, mobile_ref)
      end
    end)
  end

  def random_50(room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      roll = :rand.uniform(100)

      cond do
        roll <= 80 ->
          Mobile.send_scroll(mobile, "<p>Nothing.</p>")

          Room.send_scroll(
            room,
            "<p>Nothing.</p>",
            [mobile]
          )

          room

        roll <= 81 ->
          # longsword
          give_item(room, mobile_ref, 64)

        roll <= 82 ->
          # hatchet
          give_item(room, mobile_ref, 75)

        roll <= 83 ->
          # mace
          give_item(room, mobile_ref, 87)

        roll <= 84 ->
          # warhammer
          give_item(room, mobile_ref, 91)

        roll <= 85 ->
          # maul
          give_item(room, mobile_ref, 171)

        roll <= 86 ->
          # quarterstaff
          give_item(room, mobile_ref, 100)

        roll <= 87 ->
          # battle hammer
          give_item(room, mobile_ref, 103)

        roll <= 88 ->
          # club
          give_item(room, mobile_ref, 90)

        roll <= 89 ->
          # falchion
          give_item(room, mobile_ref, 76)

        roll <= 90 ->
          # bastard sword
          give_item(room, mobile_ref, 80)

        roll <= 91 ->
          # cloth pants
          give_item(room, mobile_ref, 20)

        roll <= 92 ->
          # skeleton
          summon(room, 11)

        roll <= 93 ->
          # leather skirt
          give_item(room, mobile_ref, 441)

        roll <= 94 ->
          # padded vest
          give_item(room, mobile_ref, 332)

        roll <= 95 ->
          # rigid leather tunic
          give_item(room, mobile_ref, 12)

        roll <= 96 ->
          # padded black boots
          give_item(room, mobile_ref, 46)

        roll <= 97 ->
          # hard leather helm
          give_item(room, mobile_ref, 33)

        roll <= 98 ->
          # skullcap
          give_item(room, mobile_ref, 30)

        roll <= 99 ->
          # leather pants
          give_item(room, mobile_ref, 22)

        roll <= 100 ->
          random_90(room, mobile_ref)
      end
    end)
  end

  def random_70(room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      roll = :rand.uniform(100)

      cond do
        roll <= 80 ->
          Mobile.send_scroll(mobile, "<p>Nothing.</p>")

          Room.send_scroll(
            room,
            "<p>Nothing.</p>",
            [mobile]
          )

          room

        roll <= 81 ->
          # longsword
          give_item(room, mobile_ref, 64)

        roll <= 82 ->
          # trident
          give_item(room, mobile_ref, 452)

        roll <= 83 ->
          # falchion
          give_item(room, mobile_ref, 76)

        roll <= 84 ->
          # battle axe
          give_item(room, mobile_ref, 71)

        roll <= 85 ->
          # halberd
          give_item(room, mobile_ref, 82)

        roll <= 86 ->
          # warhammer
          give_item(room, mobile_ref, 91)

        roll <= 87 ->
          # bladed staff
          give_item(room, mobile_ref, 461)

        roll <= 88 ->
          # morning-star
          give_item(room, mobile_ref, 65)

        roll <= 89 ->
          # zombie
          summon(room, 12)

        roll <= 90 ->
          # red robes
          give_item(room, mobile_ref, 273)

        roll <= 91 ->
          # warhammer
          give_item(room, mobile_ref, 91)

        roll <= 92 ->
          # silk robe
          give_item(room, mobile_ref, 289)

        roll <= 93 ->
          # cloth pants
          give_item(room, mobile_ref, 20)

        roll <= 94 ->
          # silk cape
          give_item(room, mobile_ref, 55)

        roll <= 95 ->
          # silk gloves
          give_item(room, mobile_ref, 429)

        roll <= 99 ->
          # skeleton
          summon(room, 11)

        roll <= 100 ->
          random_90(room, mobile_ref)
      end
    end)
  end

  def random_90(room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      roll = :rand.uniform(100)

      cond do
        roll <= 9 ->
          # glowing warhammer
          give_item(room, mobile_ref, 116)

        roll <= 12 ->
          # cloth pants
          give_item(room, mobile_ref, 20)

        roll <= 13 ->
          # chainmail halter
          give_item(room, mobile_ref, 416)

        roll <= 14 ->
          # wolf helm
          give_item(room, mobile_ref, 403)

        roll <= 15 ->
          # padded vest
          give_item(room, mobile_ref, 332)

        roll <= 16 ->
          # golden armbands
          give_item(room, mobile_ref, 170)

        roll <= 19 ->
          # black scimitar
          give_item(room, mobile_ref, 458)

        roll <= 20 ->
          # glowing broadsword
          give_item(room, mobile_ref, 361)

        roll <= 24 ->
          # crossbow
          give_item(room, mobile_ref, 623)

        roll <= 25 ->
          # black scimitar
          give_item(room, mobile_ref, 458)

        roll <= 29 ->
          # black shield
          give_item(room, mobile_ref, 180)

        roll <= 33 ->
          # padded helm
          give_item(room, mobile_ref, 334)

        roll <= 37 ->
          # sabre
          give_item(room, mobile_ref, 514)

        roll <= 40 ->
          # black robes
          give_item(room, mobile_ref, 274)

        roll <= 50 ->
          # hooded mask
          give_item(room, mobile_ref, 367)

        roll <= 55 ->
          # padded black boots
          give_item(room, mobile_ref, 46)

        roll <= 70 ->
          # 2 zombies and a skeleton
          room
          |> summon(12)
          |> summon(12)
          |> summon(11)

        roll <= 99 ->
          Mobile.send_scroll(mobile, "<p>Nothing.</p>")

          Room.send_scroll(
            room,
            "<p>Nothing.</p>",
            [mobile]
          )

          room

        roll <= 100 ->
          room
          |> random_90(mobile_ref)
          |> random_90(mobile_ref)
      end
    end)
  end
end
