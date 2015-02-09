defmodule ApathyDrive.Exit do
  use Systems.Reload
  import Utility

  def direction(direction) do
    case direction do
      "n" ->
        "north"
      "ne" ->
        "northeast"
      "e" ->
        "east"
      "se" ->
        "southeast"
      "s" ->
        "south"
      "sw" ->
        "southwest"
      "w" ->
        "west"
      "nw" ->
        "northwest"
      "u" ->
        "up"
      "d" ->
        "down"
      direction ->
        direction
    end
  end

  def look(%Spirit{} = spirit, direction) do
    current_room = Spirit.find_room(spirit)
    room_exit = current_room |> get_exit_by_direction(direction)
    look(current_room, spirit, room_exit)
  end

  def look(%Monster{} = monster, direction) do
    current_room = Monster.find_room(monster)
    room_exit = current_room |> get_exit_by_direction(direction)
    look(current_room, monster, room_exit)
  end

  def look(_current_room, %Spirit{} = spirit, nil) do
    Phoenix.Channel.reply spirit.socket, "scroll", %{:html => "<p>There is no exit in that direction.</p>"}
  end

  def look(_current_room, %Monster{} = monster, nil) do
    Phoenix.Channel.broadcast "monsters:#{monster.id}", "scroll", %{:html => "<p>There is no exit in that direction.</p>"}
  end

  def look(%Room{} = current_room, %Spirit{} = spirit, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".look(current_room, spirit, room_exit)
  end

  def look(%Room{} = current_room, %Monster{} = monster, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".look(current_room, monster, room_exit)
  end

  def move(%Spirit{} = spirit, direction) do
    current_room = Spirit.find_room(spirit)
    room_exit = current_room |> get_exit_by_direction(direction)
    move(current_room, spirit, room_exit)
  end

  def move(%Monster{} = monster, direction) do
    current_room = Monster.find_room(monster)
    room_exit = current_room |> get_exit_by_direction(direction)
    move(current_room, monster, room_exit)
  end

  def move(_current_room, %Spirit{} = spirit, nil) do
    Spirit.send_scroll(spirit, "<p>There is no exit in that direction.</p>")
  end

  def move(_current_room, %Monster{} = monster, nil) do
    Monster.send_scroll(monster, "<p>There is no exit in that direction.</p>")
  end

  def move(current_room, spirit_or_monster, room_exit) do
    :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".move(current_room, spirit_or_monster, room_exit)
  end

  def get_exit_by_direction(%Room{exits: exits} = room, direction) do
    Enum.find(exits, &(&1["direction"] == direction(direction)))
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

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import BlockTimer
      alias ApathyDrive.Exit

      def display_direction(_room, room_exit) do
        room_exit["direction"]
      end

      def move(current_room, %Spirit{} = spirit, room_exit) do
        new_room = Room.find(room_exit["destination"])
                   |> Room.value

        Room.look(new_room, spirit)

        spirit
        |> Spirit.set_room_id(room_exit["destination"])
        |> Spirit.deactivate_hint("movement")
        |> Spirit.save
      end

      def move(nil, monster, current_room, room_exit) do
        if !Systems.Combat.stunned?(monster) do
          destination = Room.find(room_exit["destination"])
          Components.Monsters.remove_monster(current_room, monster)
          Components.Monsters.add_monster(destination, monster)
          if Entity.has_component?(monster, Components.ID) do
            Entities.save!(destination)
            Entities.save!(current_room)
          end
          Entities.save(monster)
          notify_monster_left(monster, current_room, destination)
          notify_monster_entered(monster, current_room, destination)
        end
      end

      def move(spirit, monster, current_room, room_exit) do
        if Systems.Combat.stunned?(monster) do
          send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
        else
          destination = Room.find(room_exit["destination"])
          Components.Monsters.remove_monster(current_room, monster)
          Components.Monsters.add_monster(destination, monster)
          Components.Characters.remove_character(current_room, spirit)
          Components.Characters.add_character(destination, spirit)
          Entities.save!(destination)
          Entities.save!(current_room)
          Entities.save!(spirit)
          Entities.save(monster)
          notify_monster_left(monster, current_room, destination)
          notify_monster_entered(monster, current_room, destination)
          Spirit.deactivate_hint(spirit, "movement")
          Systems.Room.display_room_in_scroll(spirit, monster, destination)
        end
      end

      def look(%Room{} = room, %Spirit{} = spirit, room_exit) do
        {mirror_room, mirror_exit} = mirror(room, room_exit)

        Room.look(mirror_room, spirit)
      end

      def look(%Room{} = room, %Monster{} = monster, room_exit) do
        {mirror_room, mirror_exit} = mirror(room, room_exit)

        Room.look(mirror_room, monster)

        if mirror_exit do
          message = "#{monster.name} peeks in from #{Room.enter_direction(mirror_exit["direction"])}!"
                     |> capitalize_first

          Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-magenta'>#{message}</span></p>"}
        end
      end

      def notify_monster_entered(monster, entered_from, room) do
        direction = get_direction_by_destination(room, entered_from)
        if direction do
          Systems.Monster.display_enter_message(room, monster, direction)
        else
          Systems.Monster.display_enter_message(room, monster)
        end
        Systems.Aggression.monster_entered(monster, room)
      end

      def notify_monster_left(monster, room, left_to) do
        direction = get_direction_by_destination(room, left_to)
        if direction do
          Systems.Monster.display_exit_message(room, monster, direction)
          Systems.Monster.pursue(room, monster, direction)
        else
          Systems.Monster.display_exit_message(room, monster)
        end
      end

      def get_direction_by_destination(room, destination) do
        exits = Components.Exits.value(room)
        exit_to_destination = exits
                              |> Enum.find fn(room_exit) ->
                                   other_room = Room.find(room_exit["destination"])
                                   other_room == destination
                                 end
        exit_to_destination["direction"]
      end

      def mirror(%Room{exits: exits, id: id} = room, %{"destination" => destination}) do
        mirror_room = Room.find(destination)
                      |> Room.value

        room_exit = exits
                    |> Enum.find(fn(%{"destination" => destination}) ->
                         destination == id
                       end)
        {mirror_room, room_exit}
      end

      defoverridable [move: 3,
                      look: 3,
                      display_direction: 2]
    end
  end

end
