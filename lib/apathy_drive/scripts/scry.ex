defmodule ApathyDrive.Scripts.Scry do
  alias ApathyDrive.{Mobile, Room, RoomServer}

  def execute(%Room{} = room, mobile_ref, {:scry, %{room_id: nil}}) do
    Room.update_mobile(room, mobile_ref, fn _room, character ->
      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>You fail to locate your target.</span></p>"
      )

      character
    end)
  end

  def execute(%Room{} = room, mobile_ref, {:scry, mobile}) do
    Room.update_mobile(room, mobile_ref, fn _room, character ->
      if mobile.room_id == character.room_id do
        target = room.mobiles[mobile.ref]

        if target do
          ApathyDrive.Commands.Look.execute(room, character, ignore_light: true)

          Mobile.send_scroll(
            target,
            "<p><span class='dark-magenta'>You feel the presence of another mind.</span></p>"
          )
        end
      else
        mobile.room_id
        |> RoomServer.find()
        |> RoomServer.look(character, ignore_light: true)

        mobile.room_id
        |> RoomServer.find()
        |> RoomServer.scry(mobile.ref)
      end

      character =
        character
        |> Systems.Effect.add(
          %{
            "Scry" =>
              {mobile.__struct__, Map.get(mobile, :room_monster_id) || Map.get(mobile, :id)},
            "stack_key" => :scry,
            "stack_count" => 1,
            "StatusMessage" => "Your mind is locked on to #{mobile.name}.",
            "RemoveMessage" => "Your awareness of #{mobile.name} fades."
          },
          :timer.seconds(240)
        )

      Mobile.send_scroll(
        character,
        "<p span class='blue'>Your mind is locked on to #{mobile.name}.</p>"
      )

      character
    end)
  end
end
