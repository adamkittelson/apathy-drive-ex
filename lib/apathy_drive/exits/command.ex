defmodule ApathyDrive.Exits.Command do
  use ApathyDrive.Exit
  alias ApathyDrive.Mobile

  def display_direction(_room, room_exit) do
    room_exit["name"]
  end

  def move(_room, mobile, _room_exit)  do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def move_via_command(current_room, mobile, %{"destination" => destination_id} = room_exit) do
    destination = Room.find(destination_id)

    if room_exit["to_message"] do
      Room.send_scroll(destination, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>")
    else
      notify_mobile_entered(mobile, current_room, destination)
    end

    send(mobile, {:move_to, destination_id})

    if room_exit["mover_message"] do
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{room_exit["mover_message"]}</span></p>")
    end

    Commands.Look.look_at_room(mobile)

    if room_exit["from_message"] do
      Room.send_scroll(current_room, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => %{name: Mobile.look_name(mobile)}})}</span></p>")
    else
      notify_mobile_left(mobile, current_room, destination)
    end
  end

  def look(_room, mobile, _room_exit)  do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

end
