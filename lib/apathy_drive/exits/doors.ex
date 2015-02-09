defmodule ApathyDrive.Exits.Doors do

  defmacro __using__(_opts) do
    quote do
      use ApathyDrive.Exit

      def name, do: "door"

      def display_direction(%Room{} = room, room_exit) do
        case open?(room, room_exit) do
          true ->
            "open #{name} #{room_exit["direction"]}"
          false ->
            "closed #{name} #{room_exit["direction"]}"
        end
      end

      def move(current_room, %Spirit{} = spirit, room_exit) do
        new_room = Room.find(room_exit["destination"])
                   |> Room.value

        if !open?(current_room, room_exit) do
          Spirit.send_scroll(spirit, "<p><span class='dark-green'>You pass right through the #{name}.</span></p>")
        end

        Room.look(new_room, spirit)

        spirit
        |> Spirit.set_room_id(room_exit["destination"])
        |> Spirit.deactivate_hint("movement")
        |> Spirit.save
      end

      def move(current_room, %Monster{} = monster, room_exit) do
        if open?(current_room, room_exit) do
          super(current_room, monster, room_exit)
        else
          send_message(monster, "scroll", "<p><span class='red'>The #{name} is closed!</span></p>")
        end
      end

      def bash?(monster, room_exit) do
        :random.seed(:os.timestamp)
        Systems.Stat.modified(monster, "strength") + room_exit.difficulty >= :random.uniform(100)
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

            msg = "You see #{Components.Name.value(monster)} bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}."
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
            msg = "You see #{Components.Name.value(monster)} attempt to bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}."
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

            msg = "You see #{Components.Name.value(monster)} open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}."
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

            if (skill + room_exit.difficulty >= :random.uniform(100)) do
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
                  |> Components.Items.get_items
                  |> Enum.find(fn(item) ->
                       Components.Name.value(item) == room_exit.key
                     end)

            if key do
              unlock!(room, room_exit["direction"])
              send_message(monster, "scroll", "<p>You unlocked the #{name} with your #{Components.Name.value(key)}.</p>")
              Components.Uses.use!(key)
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
               send_message(character, "scroll", "<p>The #{name} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>")
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
               send_message(character, "scroll", "<p>The #{name} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened.</p>")
             end)
        end
      end

      def open!(room, direction) do
        if Components.Exits.get_open_duration(room, direction) do
          Systems.Effect.add(room, %{open: direction}, Components.Exits.get_open_duration(room, direction))
          # todo: tell players in the room when it re-locks
          #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
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
        #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
      end

      def close(monster, room, room_exit) do
        if open?(room, room_exit) do
          close!(room, room_exit["direction"])
          send_message(monster, "scroll", "<p>The #{name} is now closed.</p>")

          msg = "You see #{Components.Name.value(monster)} close the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}."
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

            msg = "You see #{Components.Name.value(monster)} lock the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}."
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
               send_message(character, "scroll", "<p>The #{name} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>")
             end)
        end
      end

      def lock!(room, direction) do
        effects = Room.effects(room)

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
               send_message(character, "scroll", "<p>The #{name} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed.</p>")
             end)
        end
      end

      def close!(room, direction) do
        effects = Room.effects(room)

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
        Room.effects(room)
        |> Map.values
        |> Enum.filter(fn(effect) ->
             Map.has_key?(effect, :unlocked)
           end)
        |> Enum.map(fn(effect) ->
             Map.get(effect, :unlocked)
           end)
        |> Enum.member?(room_exit["direction"])
      end

      def open?(%Room{} = room, room_exit) do
        permanently_open?(room, room_exit) or
        all_remote_actions_triggered?(room, room_exit) or
        temporarily_open?(room, room_exit) or
        opened_remotely?(room, room_exit)
      end

      def permanently_open?(room, room_exit) do
        !!room_exit[:open]
      end

      def all_remote_actions_triggered?(room, room_exit) do
        if room_exit[:remote_action_exits] do
          room_exit.remote_action_exits
          |> Enum.all?(fn(remote_exit) ->
               Room.find(remote_exit.room)
               |> Room.effects
               |> Map.values
               |> Enum.filter(fn(effect) ->
                    Map.has_key?(effect, :triggered)
                  end)
               |> Enum.map(fn(effect) ->
                    Map.get(effect, :triggered)
                  end)
               |> Enum.member?(remote_exit["direction"])
             end)
        else
          false
        end
      end

      def temporarily_open?(%Room{} = room, room_exit) do
        room.effects
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

      defoverridable [name: 0, bash?: 2]

    end
  end

end
