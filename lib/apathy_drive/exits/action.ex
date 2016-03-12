defmodule ApathyDrive.Exits.Action do
  use ApathyDrive.Exit

  def move(current_room, mobile, %{"destination" => destination_id} = room_exit, last_room) do
    ApathyDrive.PubSub.broadcast! "rooms:#{destination_id}:mobiles", {:mobile_movement, %{mobile: mobile, room: destination_id, message: "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>"}}

    send(mobile, {:move_to, destination_id, last_room})

    Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{room_exit["mover_message"]}</span></p>")

    Commands.Look.look_at_room(mobile, destination_id)

    ApathyDrive.PubSub.broadcast! "rooms:#{Room.id(current_room)}:mobiles", {:mobile_movement, %{mobile: mobile, room: Room.id(current_room), message: "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>"}}
  end

end
