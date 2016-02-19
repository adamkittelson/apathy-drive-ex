defmodule ApathyDrive.Exits.Action do
  use ApathyDrive.Exit

  def move(current_room, mobile, %{"destination" => destination_id} = room_exit, last_room) do
    destination = Room.find(destination_id)

    Room.send_scroll(destination, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>")

    send(mobile, {:move_to, destination_id, last_room})

    Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{room_exit["mover_message"]}</span></p>")

    Commands.Look.look_at_room(mobile)

    Room.send_scroll(current_room, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>")
  end

end
