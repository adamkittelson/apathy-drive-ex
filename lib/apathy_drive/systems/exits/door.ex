defmodule Systems.Exits.Door do
  use Systems.Exit

  def name, do: "door"

  def display_direction(room, room_exit) do
    case open?(room, room_exit) do
      true ->
        "open #{name} #{room_exit["direction"]}"
      false ->
        "closed #{name} #{room_exit["direction"]}"
    end
  end

  def move(spirit, nil, current_room, room_exit) do
    destination = Rooms.find_by_id(room_exit["destination"])
    Components.Characters.remove_character(current_room, spirit)
    Components.Characters.add_character(destination, spirit)
    Entities.save!(destination)
    Entities.save!(current_room)
    Entities.save!(spirit)
    Components.Hints.deactivate(spirit, "movement")

    if !open?(current_room, room_exit) do
      send_message(spirit, "scroll", "<p><span class='dark-green'>You pass right through the door.</span></p>")
    end

    Systems.Room.display_room_in_scroll(spirit, destination)
  end

  def move(spirit, monster, current_room, room_exit) do
    if open?(current_room, room_exit) do
      super(spirit, monster, current_room, room_exit)
    else
      send_message(spirit, "scroll", "<p><span class='red'>The #{name} is closed!</span></p>")
    end
  end

  def bash?(monster, room_exit) do
    :random.seed(:os.timestamp)
    Systems.Stat.modified(monster, "strength") + room_exit["difficulty"] >= :random.uniform(100)
  end

  def damage(monster) do
    amount = monster
             |> Systems.Damage.base_attack_damage
             |> Systems.Damage.raw_damage

    send_message(monster, "scroll", "<p>You take #{amount} damage for bashing the #{name}!</p>")
    Systems.Damage.do_damage(monster, amount)
  end

  def bash(monster, room, room_exit) do
    cond do
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already open.</p>")
      bash?(monster, room_exit) ->
        open!(room, room_exit["direction"])
        send_message(monster, "scroll", "<p>You bashed the #{name} open.</p>")

        msg = "You see #{Components.Name.value(monster)} bash open the #{name} #{Exit.direction_description(room_exit["direction"])}."
        room
        |> Systems.Room.characters_in_room
        |> Enum.each(fn(character) ->
             observer = Possession.possessed(character) || character

             if observer != monster do
               send_message(observer, "scroll", "<p>#{msg}</p>")
             end
           end)

        mirror_bash(room, room_exit)
      true ->
        send_message(monster, "scroll", "<p>Your attempts to bash through fail!</p>")
        msg = "You see #{Components.Name.value(monster)} attempt to bash open the #{name} #{Exit.direction_description(room_exit["direction"])}."
        room
        |> Systems.Room.characters_in_room
        |> Enum.each(fn(character) ->
             observer = Possession.possessed(character) || character

             if observer != monster do
               send_message(observer, "scroll", "<p>#{msg}</p>")
             end
           end)
        :random.seed(:os.timestamp)
        if :random.uniform(3) == 3 do
          damage(monster)
        end
    end
  end

  def open(monster, room, room_exit) do
    cond do
      locked?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is locked.</p>")
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} was already open.</p>")
      true ->
        open!(room, room_exit["direction"])
        send_message(monster, "scroll", "<p>The #{name} is now open.</p>")

        msg = "You see #{Components.Name.value(monster)} open the #{name} #{Exit.direction_description(room_exit["direction"])}."
        room
        |> Systems.Room.characters_in_room
        |> Enum.each(fn(character) ->
             observer = Possession.possessed(character) || character

             if observer != monster do
               send_message(observer, "scroll", "<p>#{msg}</p>")
             end
           end)

        mirror_open(room, room_exit)
    end
  end

  def pick(monster, room, room_exit) do
    cond do
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already open.</p>")
      !locked?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already unlocked.</p>")
      true ->
        :random.seed(:os.timestamp)
        skill = (Skills.Stealth.modified(monster) + Skills.Perception.modified(monster)) / 3

        if (skill + room_exit["difficulty"] >= :random.uniform(100)) do
          unlock!(room, room_exit["direction"])
          send_message(monster, "scroll", "<p>You successfully unlocked the #{name}.</p>")
          mirror_unlock(room, room_exit)
        else
          send_message(monster, "scroll", "<p>Your skill fails you this time.</p>")
        end
    end
  end

  def unlock(monster, room, room_exit) do
    cond do
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already open.</p>")
      !locked?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already unlocked.</p>")
      true ->
        key = monster
              |> Components.Items.value
              |> Enum.find(fn(item) ->
                   Components.Name.value(item) == room_exit["key"]
                 end)

        if key do
          unlock!(room, room_exit["direction"])
          send_message(monster, "scroll", "<p>You unlocked the #{name} with your #{Components.Name.value(key)}.</p>")
          mirror_unlock(room, room_exit)
        else
          send_message(monster, "scroll", "<p>None of your keys seem to fit this lock.</p>")
        end
    end
  end

  def mirror_bash(room, room_exit) do
    {mirror_room, mirror_exit} = mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] and !open?(mirror_room, mirror_exit) do
      open!(mirror_room, mirror_exit["direction"])

      mirror_room
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           send_message(character, "scroll", "<p>The #{name} #{Exit.direction_description(mirror_exit["direction"])} just flew open!</p>")
         end)
    end
  end

  def mirror_unlock(room, room_exit) do
    {mirror_room, mirror_exit} = mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] and !open?(mirror_room, mirror_exit) do
      unlock!(mirror_room, mirror_exit["direction"])
    end
  end

  def mirror_open(room, room_exit) do
    {mirror_room, mirror_exit} = mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] and !open?(mirror_room, mirror_exit) do
      open!(mirror_room, mirror_exit["direction"])

      mirror_room
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           send_message(character, "scroll", "<p>The #{name} #{Exit.direction_description(mirror_exit["direction"])} just opened.</p>")
         end)
    end
  end

  def open!(room, direction) do
    if Components.Exits.get_open_duration(room, direction) do
      Systems.Effect.add(room, %{open: direction}, Components.Exits.get_open_duration(room, direction))
      # todo: tell players in the room when it re-locks
      #"The #{name} #{Exit.direction_description(exit.direction)} just locked!"
    else
      Components.Exits.open_door(room, direction)
    end
  end

  def unlock!(room, direction) do
    unlock_duration = if Components.Exits.get_open_duration(room, direction) do
      Components.Exits.get_open_duration(room, direction)
    else
      300
    end

    Systems.Effect.add(room, %{unlocked: direction}, unlock_duration)
    # todo: tell players in the room when it re-locks
    #"The #{name} #{Exit.direction_description(exit.direction)} just locked!"
  end

  def close(monster, room, room_exit) do
    if open?(room, room_exit) do
      close!(room, room_exit["direction"])
      send_message(monster, "scroll", "<p>The #{name} is now closed.</p>")

      msg = "You see #{Components.Name.value(monster)} close the #{name} #{Exit.direction_description(room_exit["direction"])}."
      room
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           observer = Possession.possessed(character) || character

           if observer != monster do
             send_message(observer, "scroll", "<p>#{msg}</p>")
           end
         end)
      mirror_close(room, room_exit)
    else
      msg = "<p><span class='red'>That #{name} is already closed.</span></p>"
      send_message(monster, "scroll", msg)
    end
  end

  def lock(monster, room, room_exit) do
    cond do
      locked?(room, room_exit) ->
        send_message(monster, "scroll", "<p>The #{name} is already locked.</p>")
      open?(room, room_exit) ->
        send_message(monster, "scroll", "<p>You must close the #{name} before you may lock it.</p>")
      true ->
        lock!(room, room_exit["direction"])
        send_message(monster, "scroll", "<p>The #{name} is now locked.</p>")

        msg = "You see #{Components.Name.value(monster)} lock the #{name} #{Exit.direction_description(room_exit["direction"])}."
        room
        |> Systems.Room.characters_in_room
        |> Enum.each(fn(character) ->
             observer = Possession.possessed(character) || character

             if observer != monster do
               send_message(observer, "scroll", "<p>#{msg}</p>")
             end
           end)

        mirror_lock(room, room_exit)
    end
  end

  def mirror_lock(room, room_exit) do
    {mirror_room, mirror_exit} = mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] and !open?(mirror_room, mirror_exit) do
      lock!(mirror_room, mirror_exit["direction"])

      mirror_room
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           send_message(character, "scroll", "<p>The #{name} #{Exit.direction_description(mirror_exit["direction"])} just locked!</p>")
         end)
    end
  end

  def lock!(room, direction) do
    effects = room
              |> Components.Effects.value

    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:unlocked] == direction
       end)
    |> Enum.each(&(Components.Effects.remove(room, &1)))
  end

  def mirror_close(room, room_exit) do
    {mirror_room, mirror_exit} = mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] and open?(mirror_room, mirror_exit) do
      close!(mirror_room, mirror_exit["direction"])

      mirror_room
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           send_message(character, "scroll", "<p>The #{name} #{Exit.direction_description(mirror_exit["direction"])} just closed.</p>")
         end)
    end
  end

  def close!(room, direction) do
    effects = room
              |> Components.Effects.value

    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:open] == direction
       end)
    |> Enum.each(&(Components.Effects.remove(room, &1)))

    Components.Exits.close_door(room, direction)
    unlock!(room, direction)
  end

  def locked?(room, room_exit) do
    !open?(room, room_exit) and !unlocked?(room, room_exit)
  end

  def unlocked?(room, room_exit) do
    room
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :unlocked)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :unlocked)
       end)
    |> Enum.member?(room_exit["direction"])
  end

  def open?(room, room_exit) do
    permanently_open?(room, room_exit) or
    all_remote_actions_triggered?(room, room_exit) or
    temporarily_open?(room, room_exit) or
    opened_remotely?(room, room_exit)
  end

  def permanently_open?(room, room_exit) do
    !!room_exit[:open]
  end

  def all_remote_actions_triggered?(room, room_exit) do
    false
    # if remote_action_exit_ids.present?
    #   remote_action_exit_ids.map do |exit_id|
    #     Exit.find(exit_id)
    #   end.all? do |exit|
    #     exit.remote_action_triggered?(reactor)
    #   end
    # end
  end

  def temporarily_open?(room, room_exit) do
    room
    |> Components.Effects.value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :open)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :open)
       end)
    |> Enum.member?(room_exit["direction"])
  end

  def opened_remotely?(room, room_exit) do
    false
    #!!reactor.timer(self, :opened_remotely)
  end

end
