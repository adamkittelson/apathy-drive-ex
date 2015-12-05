defmodule ApathyDrive.Exit do
  alias ApathyDrive.Mobile

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

    move(room, mobile, room_exit)
  end

  def move(_current_room, mobile, nil) do
    Mobile.send_scroll(mobile, "<p>There is no exit in that direction.</p>")
  end

  def move(room, mobile, room_exit) do
    if !Mobile.held(mobile) do
      :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".move(room, mobile, room_exit)
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
      alias ApathyDrive.Mobile

      def display_direction(_room, room_exit) do
        room_exit["direction"]
      end

      def move(current_room, mobile, %{"destination" => destination_id}) do
        destination = Room.find(destination_id)

        notify_mobile_entered(mobile, current_room, destination)

        send(mobile, {:move_to, destination_id})

        Commands.Look.look_at_room(mobile)

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
        display_enter_message(room, mobile, direction)
      end

      def display_enter_message(room, mobile, direction \\ nil) do
        message = mobile
                  |> Mobile.enter_message
                  |> interpolate(%{
                       "name" => Mobile.look_name(mobile),
                       "direction" => Room.enter_direction(direction)
                     })
                  |> capitalize_first

        ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{Room.id(room)}:mobiles", "scroll", %{:html => "<p><span class='grey'>#{message}</span></p>"}
      end

      def notify_monster_left(%Monster{} = monster, %Room{} = room, %Room{} = left_to), do: nil
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

        #ApathyDrive.PubSub.broadcast_from! self, "rooms:#{room.id}:monsters", {:monster_left, self, direction}
        ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{Room.id(room)}:mobiles", "scroll", %{:html => "<p><span class='grey'>#{message}</span></p>"}
      end

      def get_direction_by_destination(room, destination) do
        exit_to_destination = Room.exits(room)
                              |> Enum.find fn(room_exit) ->
                                   other_room = Room.find(room_exit["destination"])

                                   other_room == destination
                                 end
        exit_to_destination["direction"]
      end

      defoverridable [move: 3,
                      look: 3,
                      display_direction: 2]
    end
  end

end
