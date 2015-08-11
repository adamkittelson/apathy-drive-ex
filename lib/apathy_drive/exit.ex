defmodule ApathyDrive.Exit do
  alias ApathyDrive.Mobile

  def look(%Mobile{spirit: nil}, _direction), do: nil
  def look(%Mobile{room_id: room_id} = mobile, direction) do
    room      = Room.find(room_id)
    room_exit = Room.get_exit(room, direction)

    look(room, mobile, room_exit)
  end

  def look(_current_room, %Mobile{} = mobile, nil) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def look(room, %Mobile{} = mobile, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".look(room, mobile, room_exit)
  end

  def move(%Spirit{} = spirit, direction) do
    current_room = Spirit.find_room(spirit)
    #room_exit = current_room |> get_exit_by_direction(direction)
    #move(current_room, spirit, room_exit)
  end

  def move(%Monster{} = monster, direction) do
    current_room = Monster.find_room(monster)
    #room_exit = current_room |> get_exit_by_direction(direction)
    #move(current_room, monster, room_exit)
  end

  def move(_current_room, %Spirit{} = spirit, nil) do
    Spirit.send_scroll(spirit, "<p>There is no exit in that direction.</p>")
  end

  def move(_current_room, %Monster{} = monster, nil) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
  end

  def move(%Room{} = current_room, %Spirit{} = spirit, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".move(current_room, spirit, room_exit)
  end

  def move(%Room{} = current_room, %Monster{} = monster, room_exit) do
    if Monster.held(monster) do
      monster
    else
      :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".move(current_room, monster, room_exit)
    end
  end

  def direction_description(direction) do
    case direction do
    "up" ->
      "above you"
    "down" ->
      "below you"
    direction ->
      "to the #{direction}"
    end
  end

  def open_duration(%Room{} = room, direction) do
    #get_exit_by_direction(room, direction)["open_duration_in_seconds"]
  end

  def mirror(%Room{id: id}, %{"destination" => destination}) do
    mirror_room = Room.find(destination)
                  |> Room.value

    room_exit = mirror_room.exits
                |> Enum.find(fn(%{"destination" => destination}) ->
                     destination == id
                   end)
    {mirror_room, room_exit}
  end

  defmacro __using__(_opts) do
    quote do
      import Systems.Text
      import BlockTimer
      alias ApathyDrive.Exit

      def display_direction(_room, room_exit) do
        room_exit["direction"]
      end

      def move(%Room{} = current_room, %Spirit{} = spirit, room_exit) do
        new_room = Room.find(room_exit["destination"])
                   |> Room.value

        Room.look(new_room, spirit)

        spirit
        |> Spirit.set_room_id(room_exit["destination"])
        |> Spirit.deactivate_hint("movement")
        |> Spirit.save
      end

      def move(%Room{} = current_room, %Monster{} = monster, room_exit) do
        destination = Room.find(room_exit["destination"])
                      |> Room.value

        notify_monster_entered(monster, current_room, destination)

        monster = monster
                  |> Monster.set_room_id(destination.id)
                  |> Monster.save

        Room.look(destination, monster)

        notify_monster_left(monster, current_room, destination)
        monster
      end

      def look(room, %Mobile{} = mobile, %{"destination" => destination}) do
        mobile
        |> Map.put(:room_id, destination)
        |> Commands.Look.look_at_room()

        room_id = Room.id(room)

        mirror_exit =
          destination
          |> Room.find
          |> Room.mirror_exit(room_id)

        if mirror_exit do
          message = "#{mobile.name} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"
                     |> capitalize_first

          ApathyDrive.Endpoint.broadcast! "rooms:#{destination}:mobiles", "scroll", %{:html => "<p><span class='dark-magenta'>#{message}</span></p>"}
        end
      end

      def notify_monster_entered(%Monster{} = monster, %Room{} = entered_from, %Room{} = room) do
        direction = get_direction_by_destination(room, entered_from)
        if direction do
          Monster.display_enter_message(room, monster, direction)
        else
          Monster.display_enter_message(room, monster)
        end
      end

      def notify_monster_left(%Monster{} = monster, %Room{} = room, %Room{} = left_to) do
        direction = get_direction_by_destination(room, left_to)
        if direction do
          Monster.display_exit_message(room, monster, direction)
          ApathyDrive.PubSub.broadcast_from! self, "rooms:#{room.id}:monsters", {:monster_left, self, direction}
        else
          Monster.display_exit_message(room, monster)
        end
      end

      def get_direction_by_destination(%Room{exits: exits} = room, %Room{id: id} = destination) do
        exit_to_destination = exits
                              |> Enum.find fn(room_exit) ->
                                   other_room_id = room_exit["destination"]
                                                   |> Room.find
                                                   |> Room.id
                                   other_room_id == id
                                 end
        exit_to_destination["direction"]
      end

      defoverridable [move: 3,
                      look: 3,
                      display_direction: 2]
    end
  end

end
