defmodule Systems.Exit do
  use Systems.Reload
  import Utility

  def move(spirit, monster, direction) do
    current_room = Parent.of(spirit)
    room_exit = current_room |> get_exit_by_direction(direction)
    move(spirit, monster, current_room, room_exit)
  end

  def move(spirit, monster, _current_room, nil) do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move(spirit, monster, current_room, room_exit) do
    :"Elixir.Systems.Exits.#{room_exit["kind"]}".move(spirit, monster, current_room, room_exit)
  end

  def get_exit_by_direction(room, direction) do
    Components.Exits.value(room)[direction]
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import BlockTimer
      alias Systems.Monster

      def move(spirit, nil, current_room, room_exit) do
        destination = Rooms.find_by_id(room_exit["destination"])
        Components.Characters.remove_character(current_room, spirit)
        Components.Characters.add_character(destination, spirit)
        Entities.save!(destination)
        Entities.save!(current_room)
        Entities.save!(spirit)
        Components.Hints.deactivate(spirit, "movement")
        Systems.Room.display_room_in_scroll(spirit, destination)
      end

      def move(nil, monster, current_room, room_exit) do
        if Systems.Combat.stunned?(monster) do
          send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
        else
          destination = Rooms.find_by_id(room_exit["destination"])
          Components.Monsters.remove_monster(current_room, monster)
          Components.Monsters.add_monster(destination, monster)
          Entities.save!(destination)
          Entities.save!(current_room)
          Entities.save(monster)
          notify_monster_left(monster, current_room, destination)
          notify_monster_entered(monster, current_room, destination)
          Systems.Room.display_room_in_scroll(monster, destination)
        end
      end

      def move(spirit, monster, current_room, room_exit) do
        if Systems.Combat.stunned?(monster) do
          send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
        else
          destination = Rooms.find_by_id(room_exit["destination"])
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
          Components.Hints.deactivate(spirit, "movement")
          Systems.Room.display_room_in_scroll(monster, destination)
        end
      end

      def notify_monster_entered(monster, entered_from, room) do
        direction = get_direction_by_destination(room, entered_from)
        if direction do
          Monster.display_enter_message(room, monster, direction)
        else
          Monster.display_enter_message(room, monster)
        end
        Systems.Aggression.monster_entered(monster, room)
      end

      def notify_monster_left(monster, room, left_to) do
        direction = get_direction_by_destination(room, left_to)
        if direction do
          Monster.display_exit_message(room, monster, direction)
          Monster.pursue(room, monster, direction)
        else
          Monster.display_exit_message(room, monster)
        end
      end

      def get_direction_by_destination(room, destination) do
        exits = Components.Exits.value(room)
        exits
        |> Map.keys
        |> Enum.find fn(direction) ->
          Rooms.find_by_id(exits[direction]["destination"]) == destination
        end
      end

      #defoverridable [required_skills: 0]
    end
  end

end
