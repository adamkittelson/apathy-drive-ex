defmodule Systems.Exits.Command do
  use Systems.Exit

  def display_direction(_room, room_exit) do
    room_exit["name"]
  end

  def move(spirit, nil, _current_room, _room_exit)  do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move(nil, monster, current_room, room_exit) do
    move_via_command(nil, monster, current_room, room_exit)
  end

  def move(spirit, _monster, _current_room, _room_exit) do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

  def move_via_command(spirit, nil, current_room, room_exit) do
    destination = Rooms.find_by_id(room_exit["destination"])
    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    send_message(spirit, "scroll", "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>")
    Entities.save!(spirit)

    Components.Hints.deactivate(spirit, "movement")
    Systems.Room.display_room_in_scroll(spirit, destination)
  end

  def move_via_command(nil, monster, current_room, room_exit) do
    if !Systems.Combat.stunned?(monster) do
      destination = Rooms.find_by_id(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      Entities.save!(destination)
      Entities.save!(current_room)
      Entities.save(monster)

      if room_exit["from_message"] do
        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_exit_message(current_room, monster)
      end

      if room_exit["to_message"] do
        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_enter_message(destination, monster)
      end

      Systems.Aggression.monster_entered(monster, destination)

      Systems.Room.display_room_in_scroll(monster, destination)
    end
  end

  def move_via_command(spirit, monster, current_room, room_exit) do
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
      send_message(monster, "scroll", "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
      Entities.save!(spirit)
      Entities.save(monster)

      if room_exit["from_message"] do
        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_exit_message(current_room, monster)
      end

      if room_exit["to_message"] do
        Systems.Monster.observers(destination, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_enter_message(destination, monster)
      end

      Systems.Aggression.monster_entered(monster, destination)

      Components.Hints.deactivate(spirit, "movement")
      Systems.Room.display_room_in_scroll(monster, destination)
    end
  end

  def look(spirit, _monster, _current_room, _room_exit) do
    send_message(spirit, "scroll", "<p>There is no exit in that direction.</p>")
  end

end
