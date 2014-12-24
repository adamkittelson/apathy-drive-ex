defmodule Systems.Exits.AbilityTrap do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit) do
    if !Systems.Combat.stunned?(monster) do
      destination = Rooms.find_by_id(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      Entities.save!(destination)
      Entities.save!(current_room)
      Entities.save(monster)

      Systems.Aggression.monster_entered(monster, destination)

      detected = detect?(monster, room_exit)
      dodged   = dodge?(monster, room_exit)

      if (detected and dodged) do
        notify_monster_left(monster, current_room, destination)
        notify_monster_entered(monster, current_room, destination)
      else
        spring_trap!(monster, current_room, destination, room_exit)
      end
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

      Systems.Aggression.monster_entered(monster, destination)

      Components.Hints.deactivate(spirit, "movement")

      detected = detect?(monster, room_exit)
      dodged   = dodge?(monster, room_exit)

      cond do
        detected and dodged ->
          send_message(monster, "scroll", "<p><span class='yellow'>You detected a trap as you entered and nimbly dodged out of the way!</span></p>")
        detected and !dodged ->
          send_message(monster, "scroll", "<p><span class='yellow'>You detected a trap as you entered but were too slow to avoid it!</span></p>")
          spring_trap!(monster, current_room, destination, room_exit)
        true ->
          spring_trap!(monster, current_room, destination, room_exit)
      end

      if Process.alive?(monster) do
        Systems.Room.display_room_in_scroll(monster, destination)
      else
        Systems.Room.display_room_in_scroll(spirit, destination)
      end
      Monster.pursue(current_room, monster, room_exit["direction"])
    end
  end

  def spring_trap!(monster, current_room, destination, room_exit) do
    send_message(monster, "scroll", "<p><span class='red'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
    if room_exit["ability"] do
      case Abilities.find(room_exit["ability"]) do
        nil ->
          send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{room_exit["ability"]}</span></p>")
        ability ->
          ability.execute(monster, nil)
      end
    end

    if room_exit["from_message"] do
      Systems.Monster.observers(current_room, monster)
      |> Enum.each(fn(observer) ->
        send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
      end)
    end

    if room_exit["to_message"] do
      Systems.Monster.observers(destination, monster)
      |> Enum.each(fn(observer) ->
        send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
      end)
    end
  end

  def modifier(_room_exit) do
    200
  end

  def detect?(monster, room_exit) do
    :random.seed(:os.timestamp)
    perception = Systems.Skill.modified(monster, "perception") - modifier(room_exit)
    perception >= :random.uniform(100)
  end

  def dodge?(monster, room_exit) do
    :random.seed(:os.timestamp)
    dodge = Systems.Skill.modified(monster, "dodge") - modifier(room_exit)
    dodge >= :random.uniform(100)
  end

end
