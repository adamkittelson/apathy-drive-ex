defmodule ApathyDrive.RoomServer do
  use GenServer
  alias ApathyDrive.{Ability, Commands, LairMonster, Match, Mobile, Presence, PubSub,
                     Repo, Room, RoomSupervisor, RoomUnity, Text, TimerManager}
  use Timex
  require Logger

  def find(id) do
    case :global.whereis_name("room_#{id}") do
      :undefined ->
        load(id)
      room ->
        room
    end
  end

  def load(id) do
    case RoomSupervisor.launch(id) do
      {:error, {:already_started, pid}} ->
        pid
      {:ok, pid} ->
        pid
    end
  end

  def system(room, mobile, args) do
    GenServer.cast(room, {:system, mobile, args})
  end

  def toggle_rapid_essence_updates(room) do
    GenServer.cast(room, :toggle_rapid_essence_updates)
  end

  def find_item_for_script(room, item, mobile, script, failure_message) do
    GenServer.cast(room, {:find_item_for_script, item, mobile, script, failure_message})
  end

  def greet(room, greeter, query) do
    GenServer.cast(room, {:greet, greeter, query})
  end

  def audible_movement(room, except_direction) do
    GenServer.cast(room, {:audible_movement, except_direction})
  end

  def create_monster(room, monster_template_id) do
    GenServer.cast(room, {:create_monster, monster_template_id})
  end

  def load_present_mobiles(room) do
    GenServer.cast(room, :load_present_mobiles)
  end

  def ask(room, asker, target, question) do
    GenServer.cast(room, {:ask, asker, target, question})
  end

  def attack(room, attacker, target) do
    GenServer.cast(room, {:attacker, attacker, target})
  end

  def possess(room, query, spirit_id, class_name, socket, possessor) do
    GenServer.cast(room, {:possess, query, spirit_id, class_name, socket, possessor})
  end

  def trigger_remote_action(room, remote_action_exit, from) do
    GenServer.cast(room, {:trigger_remote_action, remote_action_exit, from})
  end

  def search(room, direction) do
    GenServer.cast(room, {:search, direction})
  end

  def bash(room, mobile, direction) do
    GenServer.cast(room, {:bash, mobile, direction})
  end

  def open(room, mobile, direction) do
    GenServer.cast(room, {:open, mobile, direction})
  end

  def lock(room, mobile, direction) do
    GenServer.cast(room, {:lock, mobile, direction})
  end

  def mirror_lock(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_lock, mirror_room_id, room_exit})
  end

  def mirror_open(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_open, mirror_room_id, room_exit})
  end

  def close(room, mobile, direction) do
    GenServer.cast(room, {:close, mobile, direction})
  end

  def mirror_close(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_close, mirror_room_id, room_exit})
  end

  def mirror_bash(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_bash, mirror_room_id, room_exit})
  end

  def mirror_bash_fail(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_bash_fail, mirror_room_id, room_exit})
  end

  def execute_command(room, mobile, command, arguments) do
    GenServer.cast(room, {:execute_command, mobile, command, arguments})
  end

  def execute_ability(room, ability, query) do
    GenServer.cast(room, {:execute_ability, self(), ability, query})
  end

  def look(room, mobile, args \\ []) do
    GenServer.cast(room, {:look, mobile, args})
  end

  def display_enter_message(room, data) do
    GenServer.cast(room, {:mobile_entered, data})
  end

  def display_exit_message(room, data) do
    GenServer.cast(room, {:mobile_left, data})
  end

  def mobile_movement(room, mobile, message) do
    GenServer.cast(room, {:mobile_movement, mobile, message})
  end

  def purify(room, amount) do
    GenServer.cast(room, {:purify, amount})
  end

  def get_item(room, mobile, item) do
    GenServer.cast(room, {:get_item, mobile, item})
  end

  def delve(room, mobile) do
    GenServer.cast(room, {:delve, mobile})
  end

  def destroy_item(room, item) do
    GenServer.call(room, {:destroy_item, item})
  end

  def add_item(room, item) do
    GenServer.cast(room, {:add_item, item})
  end

  def add_items(room, items) do
    GenServer.cast(room, {:add_items, items})
  end

  def auto_move(room, mobile, unities) do
    GenServer.cast(room, {:auto_move, mobile, unities})
  end

  def execute_room_ability(room, mobile) do
    GenServer.cast(room, {:execute_room_ability, mobile})
  end

  def mobiles_to_load(room_id) do
    require Ecto.Query

    Mobile
    |> Ecto.Query.where(room_id: ^room_id)
    |> Ecto.Query.select([m], m.id)
    |> Repo.all
  end

  def init(id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(:room_unity)
      |> Repo.preload(:area)

    room =
      room
      |> Room.set_default_essence
      |> Map.put(:essence_last_updated_at, Timex.DateTime.to_secs(Timex.DateTime.now))

    unless room.room_unity do
      room_unity =
        room
        |> Ecto.build_assoc(:room_unity, essences: %{"good" => 0, "evil" => 0, "default" => room.default_essence})
        |> Repo.save!

      room = %{room | room_unity: room_unity}
    end

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")
    PubSub.subscribe("areas:#{room.area_id}")

    load_present_mobiles(self())

    if room.lair_size && Enum.any?(LairMonster.monsters_template_ids(id)) do
      send(self, :spawn_monsters)
    end

    if room.ability_id do
      ability =
        Repo.get(Ability, room.ability_id).properties
        |> Map.put("ignores_global_cooldown", true)

      room =
        room
        |> Map.put(:room_ability, ability)
    end

    Process.send_after(self(), :save, 2000)

    room =
      room
      |> TimerManager.send_after({:report_essence, Application.get_env(:apathy_drive, :initial_essence_delay), :report_essence})
      |> TimerManager.send_every({:report_essence, 600_000, :report_essence})

    {:ok, room}
  end

  def handle_call({:destroy_item, item}, _from, %Room{items: items, item_descriptions: item_descriptions} = room) do
    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Match.one(:name_contains, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Match.one(:keyword_starts_with, item)

    cond do
      visible_item ->
        {:reply, {:cant_destroy, visible_item.name}, room}
      hidden_item ->
        {:reply, {:cant_destroy, hidden_item.name}, room}
      actual_item ->
        room =
          room
          |> Map.put(:items, List.delete(room.items, actual_item.item))
          |> Room.save!
        {:reply, {:ok, actual_item.item}, room}
      true ->
        {:reply, :not_found, room}
    end
  end

  def handle_call({:lock, direction}, _from, room) do
    room = Room.lock!(room, direction)
    {:reply, room, room}
  end

  def handle_cast({:system, mobile, command}, %Room{} = room) do
    room = Commands.System.execute(room, mobile, command)

    {:noreply, room}
  end

  def handle_cast(:toggle_rapid_essence_updates, %Room{} = room) do
    cond do
      Room.spirits_present?(room) and !TimerManager.time_remaining(room, :rapid_essence_update) ->
        {:noreply, TimerManager.send_every(room, {:rapid_essence_update, 1_000, :report_essence})}
      !Room.spirits_present?(room) and TimerManager.time_remaining(room, :rapid_essence_update) ->
        TimerManager.cancel(room, :rapid_essence_update)
        {:noreply, room}
      true ->
        {:noreply, room}
    end
  end

  def handle_cast({:execute_room_ability, mobile}, %Room{room_ability: room_ability} = room) do
    Mobile.execute_room_ability(mobile, room_ability)
    {:noreply, room}
  end

  def handle_cast({:find_item_for_script, item, mobile, script, failure_message}, %Room{} = room) do
    if Room.find_item(room, item) do
      Mobile.execute_script(mobile, script)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
    end
    {:noreply, room}
  end

  def handle_cast({:greet, greeter, query}, %Room{} = room) do
    Commands.Greet.execute(room, greeter, query)
    {:noreply, room}
  end

  def handle_cast({:audible_movement, except_direction}, %Room{exits: exits} = room) do
    exits
    |> Enum.each(fn
         %{"direction" => direction, "kind" => kind, "destination" => dest} when kind in ["Normal", "Action", "Door", "Gate"] and direction != except_direction ->
           PubSub.broadcast("rooms:#{dest}:mobiles", {:audible_movement, ApathyDrive.Exit.reverse_direction(direction)})
         _ -> :noop
       end)

    {:noreply, room}
  end
  def handle_cast({:create_monster, monster_template_id}, room) do
    monster =
      monster_template_id
      |> MobileTemplate.create_monster(room)
      |> Mobile.load

    Mobile.display_enter_message(monster, self())

    {:noreply, room}
  end

  def handle_cast(:load_present_mobiles, room) do
    room.id
    |> mobiles_to_load()
    |> Enum.each(fn(mobile_id) ->
         mobile_id
         |> Mobile.load
         |> Mobile.display_enter_message(self())

       end)
    {:noreply, room}
  end

  def handle_cast({:attacker, attacker, target}, room) do
    Commands.Attack.execute(room, attacker, target)
    {:noreply, room}
  end

  def handle_cast({:ask, asker, target, question}, room) do
    Commands.Ask.execute(room, asker, target, question)
    {:noreply, room}
  end

  def handle_cast({:possess, query, spirit_id, class_name, socket, possessor}, room) do
    Commands.Possess.execute(room, query, spirit_id, class_name, socket, possessor)
    {:noreply, room}
  end

  def handle_cast({:get_item, mobile, item}, room) do
    {:noreply, Commands.Get.execute(room, mobile, item)}
  end

  def handle_cast({:delve, mobile}, room) do
    Commands.Delve.execute(room, mobile)
    {:noreply, room}
  end

  def handle_cast({:trigger_remote_action, remote_action_exit, from}, room) do
    room = Commands.RemoteAction.execute(room, remote_action_exit, from)
    {:noreply, room}
  end

  def handle_cast({:search, direction}, room) do
    room = Systems.Effect.add(room, %{searched: direction}, 300)
    {:noreply, room}
  end

  def handle_cast({:mirror_lock, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>"}
      {:noreply, Room.lock!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_close, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed!</p>"}
      {:noreply, Room.close!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_open, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened!</p>"}
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>"}
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash_fail, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} shudders from an impact, but it holds!</p>"}
    end
    {:noreply, room}
  end

  def handle_cast({:bash, mobile, direction}, room) do
    room = Commands.Bash.execute(room, mobile, direction)
    {:noreply, room}
  end

  def handle_cast({:close, mobile, direction}, room) do
    room = Commands.Close.execute(room, mobile, direction)
    {:noreply, room}
  end

  def handle_cast({:open, mobile, direction}, room) do
    room = Commands.Open.execute(room, mobile, direction)
    {:noreply, room}
  end

  def handle_cast({:lock, mobile, direction}, room) do
    room = Commands.Lock.execute(room, mobile, direction)
    {:noreply, room}
  end

  def handle_cast({:execute_command, mobile, command, arguments}, room) do
    ApathyDrive.Command.execute(room, mobile, command, arguments)
    {:noreply, room}
  end

  def handle_cast({:execute_ability, mobile, %{"kind" => kind} = ability, query}, %Room{} = room) when kind in ["attack", "curse"] do
    mobiles =
      Presence.metas("rooms:#{room.id}:mobiles")

    mobile =
      mobiles
      |> Enum.find(&(&1.mobile == mobile))

   target =
     mobiles
     |> Enum.reject(&(&1 == mobile))
     |> Match.one(:name_contains, query)

    send(mobile.mobile, {:execute_ability, ability, List.wrap(target && target.mobile)})

    {:noreply, room}
  end

  def handle_cast({:execute_ability, mobile, %{"kind" => kind} = ability, _query}, %Room{} = room) when kind in ["room attack", "room curse"] do
    mobiles =
      Presence.metas("rooms:#{room.id}:mobiles")

    mobile =
      mobiles
      |> Enum.find(&(&1.mobile == mobile))

   targets =
     mobiles
     |> Enum.reject(&(&1.unities == mobile.unities))
     |> Enum.map(&(&1.mobile))

    send(mobile.mobile, {:execute_ability, ability, targets})

    {:noreply, room}
  end

  def handle_cast({:execute_ability, mobile, %{"kind" => _kind} = ability, _query}, %Room{} = room) do
    send(mobile, {:execute_ability, ability, [mobile]})

    {:noreply, room}
  end

  def handle_cast({:look, mobile, args}, %Room{} = room) do
    Commands.Look.execute(room, mobile, args)

    {:noreply, room}
  end

  def handle_cast({:auto_move, mobile, unities}, %Room{} = room) do
    case room.exits do
      nil ->
        Mobile.auto_move(mobile, [])
      _exits ->
        valid_exits =
          case unities do
            [] ->
              exits_in_area(room)
            unities ->
              if room.room_unity.controlled_by in unities do
                case non_unity_controlled_exits(room, unities) do
                  [] ->
                    unity_controlled_exits(room, unities)
                  result ->
                    result
                  end
              else
                unity_controlled_exits(room, unities)
              end
          end
      Mobile.auto_move(mobile, valid_exits)
    end

    {:noreply, room}
  end

  def handle_cast({:mobile_movement, _mobile, _message}, room) do
    {:noreply, room}
  end

  def handle_cast({:mobile_entered, %{name: name, mobile: mobile, message: message, from: from_room_id}}, room) do
    message = message
              |> Text.interpolate(%{
                   "name" => name,
                   "direction" => room |> Room.get_direction_by_destination(from_room_id) |> Room.enter_direction()
                 })
              |> Text.capitalize_first

    ApathyDrive.PubSub.broadcast! "rooms:#{room.id}:mobiles", {:mobile_movement, %{mobile: mobile, room: room.id, message: "<p><span class='grey'>#{message}</span></p>"}}
    {:noreply, room}
  end

  def handle_cast({:mobile_left, %{name: name, mobile: mobile, message: message, to: to_room_id}}, room) do
    message = message
              |> Text.interpolate(%{
                   "name" => name,
                   "direction" => room |> Room.get_direction_by_destination(to_room_id) |> Room.exit_direction
                 })
              |> Text.capitalize_first

    room =
      room
      |> Room.update_essence

      ApathyDrive.PubSub.broadcast! "rooms:#{room.id}:mobiles", {:mobile_movement, %{mobile: mobile, room: room.id, message: "<p><span class='grey'>#{message}</span></p>"}}
    {:noreply, room}
  end

  def handle_cast({:add_item, item}, %Room{items: items} = room) do
    room =
      put_in(room.items, [item | items])
      |> Room.save!

    {:noreply, room}
  end

  def handle_cast({:add_items, new_items}, %Room{items: items} = room) do
    room =
      put_in(room.items, new_items ++ items)
      |> Repo.save

    {:noreply, room}
  end

  def handle_info({:update_area, area}, room) do
    PubSub.unsubscribe("areas:#{room.area_id}")
    room = Room.update_area(room, area)
    PubSub.subscribe("areas:#{area.id}")
    {:noreply, room}
  end

  def handle_info(:save, room) do
    Process.send_after(self, :save, jitter(:timer.minutes(30)))
    room = Repo.save(room)
    {:noreply, room, :hibernate}
  end

  def handle_info(:spawn_monsters,
                  %{:lair_next_spawn_at => lair_next_spawn_at} = room) do

    if DateTime.to_secs(DateTime.now) >= lair_next_spawn_at do

      room_pid = self()

      Task.start_link fn ->
        ApathyDrive.LairSpawning.spawn_lair(room, room_pid)
      end

      room = room
             |> Map.put(:lair_next_spawn_at, DateTime.now
                                             |> DateTime.shift(minutes: room.lair_frequency)
                                             |> DateTime.to_secs)
    end

    :erlang.send_after(5000, self, :spawn_monsters)

    {:noreply, room}
  end

  def handle_info({:door_bashed_open, %{direction: direction}}, room) do
    room = Room.open!(room, direction)

    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_bash, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_bash, room_exit}, room) do
    room = Room.open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_bash_failed, %{direction: direction}}, room) do
    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_bash_failed, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:door_opened, %{direction: direction}}, room) do
    room = Room.open!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_open, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_open, room_exit}, room) do
    room = Room.open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_closed, %{direction: direction}}, room) do
    room = Room.close!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_close, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_close, room_exit}, room) do
    room = Room.close!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_locked, %{direction: direction}}, room) do
    room = Room.lock!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_lock, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_lock, room_exit}, room) do
    room = Repo.lock!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info(:report_essence, %Room{exits: exits, room_unity: %RoomUnity{essences: essences}} = room) do
    room = Room.update_essence(room)

    Enum.each(exits, fn(%{"destination" => dest, "direction" => direction, "kind" => kind}) ->
      unless kind in ["Cast", "RemoteAction"] do
        report = %{
          essences: essences,
          room_id: room.id,
          direction: direction,
          kind: kind,
          legacy_id: room.legacy_id,
          area: room.area.name,
          controlled_by: room.room_unity.controlled_by,
          report_back?: Room.spirits_present?(room)
        }

        dest
        |> find()
        |> send({:essence_report, report})
      end
    end)

    room = Map.put(room, :essence_last_reported_at, Timex.DateTime.to_secs(Timex.DateTime.now))

    {:noreply, room}
  end

  def handle_info({:essence_report, report}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, report.room_id)

    room =
      put_in(room.room_unity.exits[mirror_exit["direction"]], %{"essences" => report.essences, "area" => report.area, "controlled_by" => report.controlled_by})

    if report.report_back? and room.essence_last_reported_at < (Timex.DateTime.to_secs(Timex.DateTime.now) - 60) do
      send(self(), :report_essence)
    end

    {:noreply, room}
  end

  def handle_info(:execute_room_ability, %Room{room_ability: ability} = room) do
    ApathyDrive.PubSub.broadcast!("rooms:#{room.id}:spirits", {:execute_room_ability, ability})

    {:noreply, room}
  end

  def handle_info({:timeout, _ref, {name, time, [module, function, args]}}, %Room{timers: timers} = room) do
    jitter = trunc(time / 2) + :rand.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, [module, function, args]})

    timers = Map.put(timers, name, new_ref)

    apply module, function, args

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, [module, function, args]}}, %Room{timers: timers} = room) do
    apply module, function, args

    timers = Map.delete(timers, name)

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, room) do
    room = Systems.Effect.remove(room, key, fire_after_cast: true, show_expiration_message: true)
    {:noreply, room}
  end

  def handle_info({:room_updated, %{changes: changes}}, room) do
    {:noreply, Map.merge(room, changes)}
  end

  def handle_info(_message, room) do
    {:noreply, room}
  end

  defp jitter(time) do
    time
    |> :rand.uniform
    |> Kernel.+(time)
  end

  defp exits_in_area(%Room{exits: exits} = room) do
    Enum.filter(exits, fn(%{"direction" => direction}) ->
      room.room_unity.exits[direction] && (room.room_unity.exits[direction]["area"] == room.area.name)
    end)
  end

  defp unity_controlled_exits(%Room{exits: exits} = room, unities) do
    Enum.filter(exits, fn(%{"direction" => direction}) ->
      room.room_unity.exits[direction] && (room.room_unity.exits[direction]["controlled_by"] in unities)
    end)
  end

  defp non_unity_controlled_exits(%Room{exits: exits} = room, unities) do
    Enum.filter(exits, fn(%{"direction" => direction}) ->
      (room.room_unity.exits[direction] && (room.room_unity.exits[direction]["area"] == room.area.name)) &&
      !(room.room_unity.exits[direction] && (room.room_unity.exits[direction]["controlled_by"] in unities))
    end)
  end

end
