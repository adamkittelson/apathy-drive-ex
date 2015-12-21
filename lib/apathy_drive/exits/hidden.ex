defmodule ApathyDrive.Exits.Hidden do
  use ApathyDrive.Exit

  def display_direction(%Room{} = room, room_exit) do
    if open?(room, room_exit) do
      room_exit["description"]
    end
  end

  def look(current_room, mobile, room_exit) do
    if open?(current_room, room_exit) or room_exit["passable_while_hidden"] do
      super(current_room, mobile, room_exit)
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    end
  end

  def search(mobile, room, room_exit) do
    if Room.searched?(room, room_exit) || !room_exit["searchable"] || :random.uniform(100) < 75 do
      Mobile.send_scroll(mobile, "<p>You notice nothing different #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")
    else
      if room_exit["message_when_revealed"] do
        Mobile.send_scroll(mobile, "<p>#{room_exit["message_when_revealed"]}</p>")
      end

      ApathyDrive.PubSub.broadcast!("rooms:#{Room.id(room)}", {:search, room_exit["direction"]})
    end
    mobile
  end

  def move(current_room, mobile, room_exit) do
    if open?(current_room, room_exit) do
      super(current_room, mobile, room_exit)
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction!</p>")
    end
  end

  def open?(room, room_exit) do
    permanently_open?(room_exit) or
    all_remote_actions_triggered?(room_exit, room) or
    Room.searched?(room, room_exit["direction"]) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room_exit) do
    !!room_exit["open"]
  end

  def all_remote_actions_triggered?(room_exit, room) do
    if room_exit["remote_action_exits"] do
      room_exit["remote_action_exits"]
      |> Enum.all?(fn(remote_exit) ->
           remote_exit["room"]
           |> Room.find
           |> Room.triggered?(remote_exit["direction"], room)
         end)
    else
      false
    end
  end

  def opened_remotely?(_room, _room_exit) do
    false
    #!!reactor.timer(self, :opened_remotely)
  end

end
