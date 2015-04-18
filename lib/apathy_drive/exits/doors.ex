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
          Monster.send_scroll(monster, "<p><span class='red'>The #{name} is closed!</span></p>")
        end
      end

      def bash?(%Monster{} = monster, room_exit) do
        :random.seed(:os.timestamp)
        80 + room_exit["difficulty"] >= :random.uniform(100)
      end

      def bash(%Monster{} = monster, %Room{} = room, room_exit) do
        cond do
          open?(room, room_exit) ->
            Monster.send_scroll(monster, "<p>The #{name} is already open.</p>")
          bash?(monster, room_exit) ->
            ApathyDrive.PubSub.broadcast!("rooms:#{room.id}",
                                     {:door_bashed_open, %{basher: monster,
                                                           direction: room_exit["direction"],
                                                           type: name }})
            monster
          true ->
            ApathyDrive.PubSub.broadcast!("rooms:#{room.id}",
                                     {:door_bash_failed, %{basher: monster,
                                                           direction: room_exit["direction"],
                                                           type: name }})
            # if :random.uniform(3) == 3 do
            #   Monster.send_scroll(monster, "<p>You take #{amount} damage for bashing the #{name}!</p>")
            # end
            monster
        end
      end

      def open(%Monster{} = monster, %Room{} = room, room_exit) do
        cond do
          locked?(room, room_exit) ->
            Monster.send_scroll(monster, "<p>The #{name} is locked.</p>")
          open?(room, room_exit) ->
            Monster.send_scroll(monster, "<p>The #{name} was already open.</p>")
          true ->
            ApathyDrive.PubSub.broadcast!("rooms:#{room.id}",
                                     {:door_opened, %{opener: monster,
                                                      direction: room_exit["direction"],
                                                      type: name }})
            monster
        end
      end

      def close(monster, room, room_exit) do
        if open?(room, room_exit) do
          ApathyDrive.PubSub.broadcast!("rooms:#{room.id}",
                                   {:door_closed, %{closer: monster,
                                                    direction: room_exit["direction"],
                                                    type: name }})
          monster
        else
          Monster.send_scroll(monster, "<p><span class='red'>That #{name} is already closed.</span></p>")
        end
      end

      def lock(monster, room, room_exit) do
        cond do
          locked?(room, room_exit) ->
            Monster.send_scroll(monster, "<p>The #{name} is already locked.</p>")
          open?(room, room_exit) ->
            Monster.send_scroll(monster, "<p>You must close the #{name} before you may lock it.</p>")
          true ->
            ApathyDrive.PubSub.broadcast!("rooms:#{room.id}",
                                     {:door_locked, %{locker: monster,
                                                      direction: room_exit["direction"],
                                                      type: name }})
            monster
        end
      end

      def locked?(%Room{} = room, room_exit) do
        !open?(room, room_exit) and !unlocked?(room, room_exit)
      end

      def unlocked?(%Room{effects: effects} = room, room_exit) do
        effects
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
        !!room_exit["open"]
      end

      def all_remote_actions_triggered?(room, room_exit) do
        if room_exit["remote_action_exits"] do
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
