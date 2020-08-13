defmodule ApathyDrive.Scripts.Scry do
  alias ApathyDrive.{Mobile, Room, RoomServer}

  def execute(%Room{} = room, mobile_ref, {nil, nil}) do
    Room.update_mobile(room, mobile_ref, fn _room, character ->
      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>You fail to locate your target.</span></p>"
      )

      character
    end)
  end

  def execute(%Room{} = room, mobile_ref, {room_id, ref}) do
    Room.update_mobile(room, mobile_ref, fn _room, character ->
      if room_id == character.room_id do
        target = room.mobiles[ref]

        if target do
          ApathyDrive.Commands.Look.execute(room, character, ignore_light: true)

          Mobile.send_scroll(
            target,
            "<p><span class='dark-magenta'>You feel the presence of another mind.</span></p>"
          )
        end
      else
        room_id
        |> RoomServer.find()
        |> RoomServer.look(character, ignore_light: true)

        room_id
        |> RoomServer.find()
        |> RoomServer.scry(ref)
      end

      character
    end)
  end
end
