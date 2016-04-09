defmodule ApathyDrive.Commands.Move do
  alias ApathyDrive.{Mobile, Doors}

  def execute(%Room{} = room, mobile, command) do
    direction = Room.direction(command)
    room_exit = Room.get_exit(room, direction)
    execute(room, mobile, room_exit, nil)
  end

  def execute(%Room{}, mobile, %{"kind" => "Trap"} = room_exit, last_room) do
    Mobile.move(mobile, self, Map.put(room_exit, "kind", "Normal"), last_room)
  end

  def execute(%Room{} = room, mobile, %{"kind" => kind} = room_exit, last_room) when kind in ["Door", "Gate"] do
    kind = String.downcase(kind)
    if Doors.open?(room, room_exit) do
      Mobile.move(mobile, self, Map.put(room_exit, "kind", "Normal"), last_room)
    else
      Mobile.send_scroll(mobile, "<p><span class='red'>The #{kind} is closed!</span></p>")
    end
  end

  def execute(%Room{}, mobile, %{"kind" => "Hidden", "passable_while_hidden" => true} = room_exit, last_room) do
    Mobile.move(mobile, self, Map.put(room_exit, "kind", "Normal"), last_room)
  end

  def execute(%Room{} = room, mobile, %{"kind" => "Hidden", "passable_while_hidden" => false} = room_exit, last_room) do
    if Doors.open?(room, room_exit) do
      Mobile.move(mobile, self, Map.put(room_exit, "kind", "Normal"), last_room)
    else
      Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
    end
  end

  def execute(%Room{}, mobile, room_exit, last_room) do
    Mobile.move(mobile, self, room_exit, last_room)
  end

  def execute(%Mobile{} = mobile, _room, nil, _last_room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def execute(%Mobile{} = mobile, _room, %{"kind" => "Command"}, _last_room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def execute(%Mobile{} = mobile, _room, %{"kind" => "RemoteAction"}, _last_room) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def execute(%Mobile{} = mobile, _room, %{"kind" => "Normal", "destination" => destination_id}, last_room) do
    import Mobile

    if !held(mobile) do

      mobile.room_id
      |> Room.find
      |> Room.display_exit_message(%{name: look_name(mobile), mobile: self, message: mobile.exit_message, to: destination_id})

      ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")
      ApathyDrive.PubSub.broadcast!("rooms:#{destination_id}:adjacent", {:audible_movement, destination_id, mobile.room_id})

      mobile =
        mobile
        |> Map.put(:room_id, destination_id)
        |> Map.put(:last_room, last_room)

      ApathyDrive.PubSub.subscribe(self, "rooms:#{destination_id}:mobiles")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{destination_id}:mobiles:#{mobile.alignment}")

      if mobile.spirit do
        ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.spirit.room_id}:spirits")
        mobile = put_in(mobile.spirit.room_id, mobile.room_id)
        ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.spirit.room_id}:spirits")
      end

      destination = Room.find(destination_id)

      Mobile.look(self)

      Room.display_enter_message(destination, %{name: look_name(mobile), mobile: self, message: mobile.enter_message, from: mobile.room_id})

      notify_presence(mobile)

      mobile

    else
      mobile
    end
  end

  def execute(%Mobile{} = mobile, _room, %{"kind" => "Action", "destination" => destination_id} = room_exit, last_room) do
    import Mobile

    if !held(mobile) do

      Mobile.send_scroll(mobile, "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>")

      mobile.room_id
      |> Room.find
      |> Room.display_exit_message(%{name: look_name(mobile), mobile: self, message: room_exit["from_message"], to: destination_id})

      ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")
      ApathyDrive.PubSub.broadcast!("rooms:#{destination_id}:adjacent", {:audible_movement, destination_id, mobile.room_id})

      mobile =
        mobile
        |> Map.put(:room_id, destination_id)
        |> Map.put(:last_room, last_room)

      ApathyDrive.PubSub.subscribe(self, "rooms:#{destination_id}:mobiles")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{destination_id}:mobiles:#{mobile.alignment}")

      if mobile.spirit do
        ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.spirit.room_id}:spirits")
        mobile = put_in(mobile.spirit.room_id, mobile.room_id)
        ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.spirit.room_id}:spirits")
      end

      destination = Room.find(destination_id)

      Mobile.look(self)

      Room.display_enter_message(destination, %{name: look_name(mobile), mobile: self, message: room_exit["to_message"], from: mobile.room_id})

      notify_presence(mobile)

      mobile

    else
      mobile
    end
  end

end
