defmodule ApathyDrive.Commands.Move do
  alias ApathyDrive.{Doors, Mobile, Room, RoomServer}

  def execute(%Room{} = room, %Mobile{} = mobile, command) when is_binary(command) do
    direction = Room.direction(command)
    room_exit = Room.get_exit(room, direction)
    execute(room, mobile, room_exit)
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => kind} = room_exit) when kind in ["Trap", "Cast"] do
    execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"] do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>You pass right through the #{String.downcase(kind)}.</span></p>")
    execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
  end
  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"] do
    if Doors.open?(room, room_exit) do
      execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(mobile, "<p><span class='red'>The #{String.downcase(kind)} is closed!</span></p>")
      room
    end
  end

  def execute(%Room{} = room, mobile, %{"kind" => "Hidden", "passable_while_hidden" => true} = room_exit) do
    execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => "Hidden", "passable_while_hidden" => false} = room_exit) do
    if Doors.open?(room, room_exit) do
      execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
      room
    end
  end

  def execute(%Room{} = room, %Mobile{} = mobile, nil) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => "Command"}) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => "RemoteAction"}) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => "Normal", "destination" => destination_id}) do
    if !Mobile.held(mobile) and !Mobile.confused(mobile) do
      Room.display_exit_message(room, %{mobile: mobile, message: mobile.exit_message, to: destination_id})

      destination_id
      |> RoomServer.find
      |> RoomServer.mobile_entered(mobile)

      put_in(room.mobiles, Map.delete(room.mobiles, mobile.ref))
      |> Room.update_essence_targets
    else
      room
    end
  end

  def execute(%Room{} = room, %Mobile{} = mobile, %{"kind" => "Action", "destination" => destination_id} = room_exit) do
    if !Mobile.held(mobile) and !Mobile.confused(mobile) do

      Mobile.send_scroll(mobile, "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>")

      destination_id
      |> RoomServer.find
      |> RoomServer.mobile_entered(mobile, "<span class='yellow'>#{room_exit["to_message"]}</span>")

      Room.display_exit_message(room, %{mobile: mobile, message: room_exit["from_message"], to: destination_id})

      put_in(room.mobiles, Map.delete(room.mobiles, mobile.ref))
      |> Room.update_essence_targets
    else
      room
    end
  end

end
