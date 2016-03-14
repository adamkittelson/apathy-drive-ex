defmodule ApathyDrive.Exit do
  alias ApathyDrive.Mobile

  def all do
    [ApathyDrive.Exits.AbilityTrap, ApathyDrive.Exits.Action,
     ApathyDrive.Exits.Cast, ApathyDrive.Exits.Command, ApathyDrive.Exits.Door,
     ApathyDrive.Exits.Doors, ApathyDrive.Exits.Gate, ApathyDrive.Exits.Hidden,
     ApathyDrive.Exits.Normal, ApathyDrive.Exits.RemoteAction,
     ApathyDrive.Exits.Trap]
  end

  def look(mobile, direction) do
    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    room_exit = Room.get_exit(room, direction)

    look(room, mobile, room_exit)
  end

  def look(_room, mobile, nil) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def look(room, mobile, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".look(room, mobile, room_exit)
  end

  def move(mobile, direction) do
    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    room_exit = Room.get_exit(room, direction)

    #move(room, mobile, room_exit, nil)
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

  def mirror(%Room{id: id}, %{"destination" => destination}) do
    mirror_room = Room.find(destination)
                  |> World.room

    room_exit = mirror_room.exits
                |> Enum.find(fn(%{"destination" => destination}) ->
                     destination == id
                   end)
    {mirror_room, room_exit}
  end

  defmacro __using__(_opts) do
    quote do
      import Systems.Text
      alias ApathyDrive.Exit
      alias ApathyDrive.Mobile

      def display_direction(_room, room_exit) do
        room_exit["direction"]
      end

      def move(current_room, mobile, %{"destination" => destination_id}, last_room) do
        destination = Room.find(destination_id)

        notify_mobile_entered(mobile, current_room, destination)

        send(mobile, {:move_to, destination_id, last_room})

        Commands.Look.look_at_room(mobile, destination_id)

        send(mobile, :notify_presence)
        notify_mobile_left(mobile, current_room, destination)
      end

      def look(room, mobile, %{"destination" => destination}) do

        Commands.Look.look_at_room(mobile, destination)

        room_id = Room.id(room)

        mirror_exit =
          destination
          |> Room.find
          |> Room.mirror_exit(room_id)

        if mirror_exit do
          message = "#{Mobile.name(mobile)} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"
                     |> capitalize_first

          ApathyDrive.Endpoint.broadcast! "rooms:#{destination}:mobiles", "scroll", %{:html => "<p><span class='dark-magenta'>#{message}</span></p>"}
        end
      end

      def notify_monster_entered(monster, room, destination), do: nil
      def notify_mobile_entered(mobile, entered_from, room) do
        direction = get_direction_by_destination(room, entered_from)
        Mobile.display_enter_message(mobile, room, direction)
      end

      def notify_mobile_left(mobile, room, left_to) do
        direction = get_direction_by_destination(room, left_to)
        display_exit_message(room, mobile, direction)
      end

      def display_exit_message(room, mobile, direction \\ nil) do
        message = mobile
                  |> Mobile.exit_message
                  |> interpolate(%{
                       "name" => Mobile.look_name(mobile),
                       "direction" => Room.exit_direction(direction)
                     })
                  |> capitalize_first

        room_id = Room.id(room)

        ApathyDrive.PubSub.broadcast! "rooms:#{room_id}:mobiles", {:mobile_movement, %{mobile: mobile, room: room_id, message: "<p><span class='grey'>#{message}</span></p>"}}
      end

      def get_direction_by_destination(room, destination) do
        exit_to_destination = Room.exits(room)
                              |> Enum.find(fn(room_exit) ->
                                   other_room = Room.find(room_exit["destination"])

                                   other_room == destination
                                 end)
        exit_to_destination["direction"]
      end

      defoverridable [move: 4,
                      look: 3,
                      display_direction: 2]
    end
  end

end
