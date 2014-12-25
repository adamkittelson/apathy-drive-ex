defmodule Systems.Exits.Cast do
  use Systems.Exit

  def move(spirit, nil, current_room, room_exit),  do: super(spirit, nil, current_room, room_exit)
  def move(nil, monster, current_room, room_exit) do
    if !Systems.Combat.stunned?(monster) do
      if room_exit["pre_exit_ability"] do
        case Abilities.find(room_exit["pre_exit_ability"]) do
          nil ->
            send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{room_exit["pre_exit_ability"]}</span></p>")
          ability ->
            ability.execute(monster, nil)
        end
      end

      destination = Rooms.find_by_id(room_exit["destination"])
      Components.Monsters.remove_monster(current_room, monster)
      Components.Monsters.add_monster(destination, monster)
      if Entity.has_component?(monster, Components.ID) do
        Entities.save!(destination)
        Entities.save!(current_room)
      end
      Entities.save(monster)

      if room_exit["from_message"] do
        Systems.Monster.observers(current_room, monster)
        |> Enum.each(fn(observer) ->
          send_message(observer, "scroll", "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
        end)
      else
        Systems.Monster.display_exit_message(current_room, monster)
      end

      if room_exit["post_exit_ability"] do
        case Abilities.find(room_exit["post_exit_ability"]) do
          nil ->
            send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{room_exit["post_exit_ability"]}</span></p>")
          ability ->
            ability.execute(monster, nil)
        end
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
      Monster.pursue(current_room, monster, room_exit["direction"])
    end
  end

  def move(spirit, monster, current_room, room_exit) do
    if Systems.Combat.stunned?(monster) do
      send_message(monster, "scroll", "<p><span class='yellow'>You are stunned and cannot move!</span></p>")
    else

      if room_exit["pre_exit_ability"] do
        case Abilities.find(room_exit["pre_exit_ability"]) do
          nil ->
            send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{room_exit["pre_exit_ability"]}</span></p>")
          ability ->
            ability.execute(monster, nil)
        end
      end

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

      if room_exit["post_exit_ability"] do
        case Abilities.find(room_exit["post_exit_ability"]) do
          nil ->
            send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{room_exit["post_exit_ability"]}</span></p>")
          ability ->
            ability.execute(monster, nil)
        end
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
      Monster.pursue(current_room, monster, room_exit["direction"])
    end
  end

  def look(spirit, monster, current_room, room_exit) do
    if room_exit["look_message"] do
      send_message(spirit, "scroll", "<p>#{room_exit["look_message"]}</p>")
    else
      super(spirit, monster, current_room, room_exit)
    end
  end

end
