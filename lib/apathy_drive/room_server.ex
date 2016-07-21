defmodule ApathyDrive.RoomServer do
  use GenServer
  alias ApathyDrive.{Commands, LairMonster, LairSpawning, Match, Mobile, Presence, PubSub, MonsterTemplate,
                     Repo, Room, RoomSupervisor, RoomUnity, Text, TimerManager}
  use Timex
  require Logger

  def find(id) do
    case Process.whereis(:"room_#{id}") do
      nil ->
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

  def send_scroll(room, html) do
    GenServer.cast(room, {:send_scroll, html})
  end

  def system(room, mobile, args) do
    GenServer.cast(room, {:system, mobile, args})
  end

  def convert?(room, unity) do
    GenServer.cast(room, {:convert?, unity})
  end

  def find_item_for_script(room, item, mobile, script, failure_message) do
    GenServer.cast(room, {:find_item_for_script, item, mobile, script, failure_message})
  end

  def greet(room, greeter, query) do
    GenServer.cast(room, {:greet, greeter, query})
  end

  def create_monster(room, monster_template_id) do
    GenServer.cast(room, {:create_monster, monster_template_id})
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

  def trigger_remote_action(room, remote_action_exit, from, opts) do
    GenServer.cast(room, {:trigger_remote_action, remote_action_exit, from, opts})
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

  def execute_command(room, spirit_id, command, arguments) do
    GenServer.cast(room, {:execute_command, spirit_id, command, arguments})
  end

  def execute_ability(room, ability, query) do
    GenServer.cast(room, {:execute_ability, self(), ability, query})
  end

  def look(room, spirit_id, args \\ []) do
    GenServer.cast(room, {:look, spirit_id, args})
  end

  def display_exit_message(room, data) do
    GenServer.cast(room, {:mobile_left, data})
  end

  def mobile_entered(room, mobile, message \\ nil) do
    GenServer.cast(room, {:mobile_entered, mobile, message})
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

  def spirit_connected(room, spirit, socket) do
    GenServer.call(room, {:spirit_connected, spirit, socket})
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

  def init(id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(:room_unity)
      |> Repo.preload(:area)

    room =
      room
      |> Room.set_default_essence

    room =
      if room.room_unity do
        room
      else
        room_unity =
          room
          |> Ecto.build_assoc(:room_unity, essences: %{"good" => 0, "evil" => 0, "default" => room.default_essence})
          |> Repo.save!

        %{room | room_unity: room_unity}
      end

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")
    PubSub.subscribe("areas:#{room.area_id}")

    send(self, :load_present_mobiles)

    if room.lair_size && Enum.any?(LairMonster.monsters_template_ids(id)) do
      send(self, :spawn_monsters)
    end

    Process.send_after(self(), :save, 2000)

    room =
      room
      #|> TimerManager.send_after({:report_essence, Application.get_env(:apathy_drive, :initial_essence_delay), :report_essence})
      |> TimerManager.send_after({:update_essence, 1_000, :update_essence})

    {:ok, room}
  end

  def handle_call({:destroy_item, item}, _from, %Room{room_unity: %RoomUnity{items: items}, item_descriptions: item_descriptions} = room) do
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
          put_in(room.room_unity.items, List.delete(room.room_unity.items, actual_item.item))
          |> Repo.save
        {:reply, {:ok, actual_item.item}, room}
      true ->
        {:reply, :not_found, room}
    end
  end

  def handle_call({:lock, direction}, _from, room) do
    room = Room.lock!(room, direction)
    {:reply, room, room}
  end

  def handle_call({:spirit_connected, %Spirit{id: id} = spirit, socket}, _from, room) do
    if mobile = Room.find_spirit(room, id) do
      room = put_in(room.mobiles[mobile.ref].socket, socket)

      room.mobiles[mobile.ref]
      |> Mobile.update_prompt

      {:reply, mobile.ref, room}
    else
      spirit = Repo.preload(spirit, :class)

      ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p>#{Spirit.look_name(spirit)} just entered the Realm.</p>"}

      mobile = Mobile.init(%Mobile{spirit: spirit, socket: socket})

      Mobile.update_prompt(mobile)

      room = put_in(room.mobiles[mobile.ref], mobile)

      {:reply, mobile.ref, room}
    end
  end

  def handle_cast({:send_scroll, html}, %Room{} = room) do
    Room.send_scroll(room, html)

    {:noreply, room}
  end

  def handle_cast({:system, mobile, command}, %Room{} = room) do
    room = Commands.System.execute(room, mobile, command)

    {:noreply, room}
  end

  def handle_cast({:convert?, unity}, %Room{room_unity: %RoomUnity{essences: essences, controlled_by: controlled_by}} = room) when unity != controlled_by do
    if LairSpawning.spawned_monster_count(room.id) == 0 do
      room =
        put_in(room.room_unity.essences, Map.merge(essences, %{controlled_by || "default" => 0, unity => room.default_essence}))
        |> Room.update_controlled_by

      {:noreply, room}
    else
      {:noreply, room}
    end
  end
  def handle_cast({:convert?, _unity}, room), do: {:noreply, room}

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

  def handle_cast({:create_monster, monster_template_id}, room) do
    monster =
      monster_template_id
      |> MonsterTemplate.create_monster(room)
      |> Mobile.load

    Mobile.display_enter_message(monster, self())

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

  def handle_cast({:trigger_remote_action, remote_action_exit, from, opts}, room) do
    room = Commands.RemoteAction.execute(room, remote_action_exit, from, opts)
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

  def handle_cast({:execute_command, mobile_ref, command, arguments}, room) do
    room = ApathyDrive.Command.execute(room, mobile_ref, command, arguments)
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

  def handle_cast({:look, spirit_id, args}, %Room{} = room) do
    Commands.Look.execute(room, spirit_id, args)

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

  def handle_cast({:mobile_entered, %Mobile{} = mobile, message}, room) do
    Room.display_enter_message(room, mobile)

    from_direction =
      room
      |> Room.get_direction_by_destination(mobile.room_id)
      |> Room.enter_direction()

    Room.send_scroll(room, "<p>#{message}</p>", mobile)

    Room.audible_movement(room, from_direction)

    mobile = Mobile.set_room_id(mobile, room.id)

    Commands.Look.execute(room, mobile, [])

    room = put_in(room.mobiles[mobile.ref], mobile)

    {:noreply, room}
  end

  def handle_cast({:add_item, item}, %Room{room_unity: %RoomUnity{items: items}} = room) do
    room =
      put_in(room.room_unity.items, [item | items])
      |> Repo.save

    {:noreply, room}
  end

  def handle_cast({:add_items, new_items}, %Room{room_unity: %RoomUnity{items: items}} = room) do
    room =
      put_in(room.room_unity.items, new_items ++ items)
      |> Repo.save

    {:noreply, room}
  end

  def handle_info(:load_present_mobiles, room) do
    {:noreply, Room.load_present_mobiles(room)}
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
    {:noreply, room}
  end

  def handle_info(:spawn_monsters,
                  %{:lair_next_spawn_at => lair_next_spawn_at} = room) do

    :erlang.send_after(5000, self, :spawn_monsters)

    if DateTime.to_secs(DateTime.now) >= lair_next_spawn_at do

      room =
        room
        |> ApathyDrive.LairSpawning.spawn_lair
        |> Map.put(
             :lair_next_spawn_at,
             DateTime.now
             |> DateTime.shift(minutes: room.lair_frequency)
             |> DateTime.to_secs
           )

      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info({:door_bashed_open, %{direction: direction}}, room) do
    room = Room.open!(room, direction)

    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = Room.mirror_exit(room, room_exit["destination"])

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

    {mirror_room, mirror_exit} = Room.mirror_exit(room, room_exit["destination"])

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_bash_failed, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:door_opened, %{direction: direction}}, room) do
    room = Room.open!(room, direction)

    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = Room.mirror_exit(room, room_exit["destination"])

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

    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = Room.mirror_exit(room, room_exit["destination"])

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

    room_exit = Room.get_exit(room, direction)

    {mirror_room, mirror_exit} = Room.mirror_exit(room, room_exit["destination"])

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_lock, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_lock, room_exit}, room) do
    room = Room.lock!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info(:update_essence, %Room{exits: _exits, room_unity: %RoomUnity{essences: essences}} = room) do
    room = Room.update_essence(room)

    room =
      if room.room_unity.essences != essences do
        TimerManager.cancel(room, :update_essence)
        TimerManager.send_after(room, {:update_essence, 1_000, :update_essence})
      else
        Room.report_essence(room)
        room
      end

    {:noreply, room}
  end

  def handle_info(:report_essence, room) do
    Room.report_essence(room)

    {:noreply, room}
  end

  def handle_info({:essence_report, report}, %Room{room_unity: room_unity} = room) do
    mirror_exit = Room.mirror_exit(room, report.room_id)

    room = put_in(room.room_unity.exits[mirror_exit["direction"]], %{"essences" => report.essences, "area" => report.area, "controlled_by" => report.controlled_by})

    room =
      if room_unity == room.room_unity do
        room
      else
        update_essence_targets(room)
      end

    {:noreply, room}
  end

  def handle_info(:update_essence_targets, room) do
    room = update_essence_targets(room)

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

  def handle_info({:room_updated, changes}, room) do
    {:noreply, Map.merge(room, changes)}
  end

  def handle_info(:room_deleted, room) do
    {:stop, :normal, room}
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
    Enum.filter(exits, fn %{"direction" => direction} = room_exit ->
      room.room_unity.exits[direction] && (room.room_unity.exits[direction]["area"] == room.area.name) && passable?(room, room_exit)
    end)
  end

  defp unity_controlled_exits(%Room{exits: exits} = room, unities) do
    Enum.filter(exits, fn %{"direction" => direction} = room_exit ->
      room.room_unity.exits[direction] && (room.room_unity.exits[direction]["controlled_by"] in unities) && passable?(room, room_exit)
    end)
  end

  defp non_unity_controlled_exits(%Room{exits: exits} = room, unities) do
    Enum.filter(exits, fn %{"direction" => direction} = room_exit ->
      (room.room_unity.exits[direction] && (room.room_unity.exits[direction]["area"] == room.area.name)) &&
      !(room.room_unity.exits[direction] && (room.room_unity.exits[direction]["controlled_by"] in unities)) && passable?(room, room_exit)
    end)
  end

  defp passable?(room, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"], do: ApathyDrive.Doors.open?(room, room_exit)
  defp passable?(_room, %{"kind" => kind}) when kind in ["Normal", "Action", "Trap", "Cast"], do: true
  defp passable?(_room, _room_exit), do: false

  defp update_essence_targets(room) do
    TimerManager.cancel(room, :update_essence)

    room
    |> Room.update_essence_targets
    |> TimerManager.send_after({:update_essence, 1_000, :update_essence})
  end

end
