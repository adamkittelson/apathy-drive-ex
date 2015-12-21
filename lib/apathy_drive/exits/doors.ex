defmodule ApathyDrive.Exits.Doors do

  defmacro __using__(_opts) do
    quote do
      use ApathyDrive.Exit
      alias ApathyDrive.Mobile
      alias ApathyDrive.PubSub

      def name, do: "door"

      def display_direction(%Room{} = room, room_exit) do
        case open?(room, room_exit) do
          true ->
            "open #{name} #{room_exit["direction"]}"
          false ->
            "closed #{name} #{room_exit["direction"]}"
        end
      end

      def move(current_room, mobile, room_exit) do
        if open?(current_room, room_exit) do
          super(current_room, mobile, room_exit)
        else
          Mobile.send_scroll(mobile, "<p><span class='red'>The #{name} is closed!</span></p>")
        end
      end

      def bash?(mobile, room_exit) do
        :random.seed(:os.timestamp)
        80 + room_exit["difficulty"] >= :random.uniform(100)
      end

      def close(mobile, room, room_exit) do
        if open?(room, room_exit) do
          Mobile.send_scroll(mobile, "<p>The #{name} is now closed.</p>")
          PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
          |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} close the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

          Room.close!(room, room_exit["direction"])

          mirror_room = Room.find(room_exit["destination"])
          mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

          if mirror_exit["kind"] == room_exit["kind"] do
            PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
            |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed.</p>")))

            Room.close!(mirror_room, mirror_exit["direction"])
          end
        else
          Mobile.send_scroll(mobile, "<p><span class='red'>That #{name} is already closed.</span></p>")
        end
      end

      def lock(mobile, room, room_exit) do
        cond do
          locked?(room, room_exit) ->
            Mobile.send_scroll(mobile, "<p>The #{name} is already locked.</p>")
          open?(room, room_exit) ->
            Mobile.send_scroll(mobile, "<p>You must close the #{name} before you may lock it.</p>")
          true ->
            Mobile.send_scroll(mobile, "<p>The #{name} is now locked.</p>")
            PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
            |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} lock the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

            Room.lock!(room, room_exit["direction"])

            mirror_room = Room.find(room_exit["destination"])
            mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

            if mirror_exit["kind"] == room_exit["kind"] do
              PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
              |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>")))

              Room.lock!(mirror_room, mirror_exit["direction"])
            end
        end
      end

      def open(mobile, room, room_exit) do
        cond do
          locked?(room, room_exit) ->
            Mobile.send_scroll(mobile, "<p>The #{name} is locked.</p>")
          open?(room, room_exit) ->
            Mobile.send_scroll(mobile, "<p>The #{name} was already open.</p>")
          true ->
            Mobile.send_scroll(mobile, "<p>The #{name} is now open.</p>")
            PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
            |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

            Room.open!(room, room_exit["direction"])

            mirror_room = Room.find(room_exit["destination"])
            mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

            if mirror_exit["kind"] == room_exit["kind"] do
              PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
              |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened.</p>")))

              Room.open!(mirror_room, mirror_exit["direction"])
            end
        end
      end

      def bash(mobile, room, room_exit) do
        cond do
          open?(room, room_exit) ->
            Mobile.send_scroll(mobile, "<p>The #{name} is already open.</p>")
          bash?(mobile, room_exit) ->
            Mobile.send_scroll(mobile, "<p>You bashed the #{name} open.</p>")
            PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
            |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

            Room.open!(room, room_exit["direction"])

            mirror_room = Room.find(room_exit["destination"])
            mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

            if mirror_exit["kind"] == room_exit["kind"] do
              PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
              |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>")))

              Room.open!(mirror_room, mirror_exit["direction"])
            end
          true ->
            Mobile.send_scroll(mobile, "<p>Your attempts to bash through fail!</p>")
            PubSub.subscribers("rooms:#{Room.id(room)}:mobiles", [mobile])
            |> Enum.each(&(Mobile.send_scroll(&1, "<p>You see #{Mobile.name(mobile)} attempt to bash open the #{name} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")))

            mirror_room = Room.find(room_exit["destination"])
            mirror_exit = Room.mirror_exit(mirror_room, Room.id(room))

            if mirror_exit["kind"] == room_exit["kind"] do
              PubSub.subscribers("rooms:#{room_exit["destination"]}:mobiles")
              |> Enum.each(&(Mobile.send_scroll(&1, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} shudders from an impact, but it holds!</p>")))

            end

            # if :random.uniform(3) == 3 do
            #   Monster.send_scroll(monster, "<p>You take #{amount} damage for bashing the #{name}!</p>")
            # end
        end
      end

      def locked?(room, room_exit) do
        !open?(room, room_exit) and !Room.unlocked?(room, room_exit["direction"])
      end

      def open?(room, room_exit) do
        permanently_open?(room_exit) or
        all_remote_actions_triggered?(room_exit) or
        Room.temporarily_open?(room, room_exit["direction"]) or
        opened_remotely?(room, room_exit)
      end

      def permanently_open?(room_exit) do
        !!room_exit["open"]
      end

      def all_remote_actions_triggered?(room_exit) do
        if room_exit["remote_action_exits"] do
          room_exit["remote_action_exits"]
          |> Enum.all?(fn(remote_exit) ->
               Room.find(remote_exit["room"])
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

      def opened_remotely?(room, room_exit) do
        false
        #!!reactor.timer(self, :opened_remotely)
      end

      defoverridable [name: 0, bash?: 2]

    end
  end

end
