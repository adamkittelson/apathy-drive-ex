defmodule ApathyDrive.Exits.AbilityTrap do
  use ApathyDrive.Exit

  def move(current_room, %Spirit{} = spirit, room_exit),  do: super(current_room, spirit, room_exit)
  def move(current_room, %Monster{} = monster, room_exit) do
    destination = Room.find(room_exit["destination"])
    Components.Monsters.remove_monster(current_room, monster)
    Components.Monsters.add_monster(destination, monster)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save(monster)

    Systems.Aggression.monster_entered(monster, destination)

    detected = detect?(monster, room_exit)
    dodged   = dodge?(monster, room_exit)

    cond do
      detected and dodged ->
        Monster.send_scroll(monster, "<p><span class='yellow'>You detected a trap as you entered and nimbly dodged out of the way!</span></p>")
      detected and !dodged ->
        Monster.send_scroll(monster, "<p><span class='yellow'>You detected a trap as you entered but were too slow to avoid it!</span></p>")
        spring_trap!(monster, current_room, destination, room_exit)
      true ->
        spring_trap!(monster, current_room, destination, room_exit)
    end

    #Systems.Room.display_room_in_scroll(spirit, monster, destination)
    Monster.pursue(current_room, monster, room_exit["direction"])
  end

  def spring_trap!(monster, current_room, destination, room_exit) do
    Monster.send_scroll(monster, "<p><span class='red'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")
    if room_exit["ability"] do
      case Abilities.find(room_exit["ability"]) do
        nil ->
          Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{room_exit["ability"]}</span></p>")
        ability ->
          ability.execute(monster, nil)
      end
    end

    if room_exit["from_message"] do
      Systems.Monster.observers(current_room, monster)
      |> Enum.each(fn(observer) ->
        Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
      end)
    end

    if room_exit["to_message"] do
      Systems.Monster.observers(destination, monster)
      |> Enum.each(fn(observer) ->
        Monster.send_scroll(observer, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")
      end)
    end
  end

  def modifier(_room_exit) do
    200
  end

  def detect?(monster, room_exit) do
    :random.seed(:os.timestamp)
    perception = Monster.modified_skill(monster, "perception") - modifier(room_exit)
    perception >= :random.uniform(100)
  end

  def dodge?(monster, room_exit) do
    :random.seed(:os.timestamp)
    dodge = Monster.modified_skill(monster, "dodge") - modifier(room_exit)
    dodge >= :random.uniform(100)
  end

end
