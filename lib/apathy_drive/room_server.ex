defmodule ApathyDrive.RoomServer do
  use GenServer

  alias ApathyDrive.{
    Ability,
    Area,
    ChannelHistory,
    Character,
    Commands,
    Directory,
    Enchantment,
    Item,
    ItemInstance,
    LairMonster,
    Mobile,
    MonsterSpawning,
    Party,
    PubSub,
    Repo,
    Room,
    RoomSupervisor,
    Shop,
    TimerManager,
    Trainer
  }

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

      false ->
        false
    end
  end

  def send_scroll(room, nil), do: room

  def send_scroll(room, html) do
    GenServer.cast(room, {:send_scroll, html})
  end

  def system(room, mobile, args) do
    GenServer.cast(room, {:system, mobile, args})
  end

  def attack(room, attacker, target) do
    GenServer.cast(room, {:attacker, attacker, target})
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

  def mirror_open_fail(room, mirror_room_id, room_exit) do
    GenServer.cast(room, {:mirror_open_fail, mirror_room_id, room_exit})
  end

  def find_mobile(room, mobile_name) do
    GenServer.call(room, {:find_mobile, mobile_name})
  end

  def scry(room, ref) do
    GenServer.call(room, {:scry, ref})
  end

  def execute_command(room, mobile_ref, command, arguments) do
    GenServer.call(room, {:execute_command, mobile_ref, command, arguments})
  end

  def enqueue_command(room, mobile_ref, command, arguments) do
    GenServer.call(room, {:enqueue_command, mobile_ref, command, arguments})
  end

  def set_chat_tab(room, mobile_ref, tab) do
    GenServer.call(room, {:set_chat_tab, mobile_ref, tab})
  end

  def tell_monsters_to_follow(room, character, destination) do
    GenServer.cast(room, {:tell_monsters_to_follow, character, destination})
  end

  def look(room, spirit_id, args \\ []) do
    GenServer.cast(room, {:look, spirit_id, args})
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

  def character_connected(room, character, socket) do
    GenServer.call(room, {:character_connected, character, socket})
  end

  def tell(room, from_character_name, to_character_name, to_character_ref, message) do
    GenServer.cast(
      room,
      {:tell, from_character_name, to_character_name, to_character_ref, message}
    )
  end

  def init(id) do
    {:ok, id, {:continue, :init}}
  end

  defp preload_area(%Room{area_id: nil} = room), do: room

  defp preload_area(%Room{area_id: area_id} = room) do
    area =
      case Repo.get(Area, area_id) do
        %Area{id: id, name: name, level: level} ->
          %Area{id: id, name: name, level: level}

        _ ->
          %Area{id: id, name: name, level: level} = Repo.get_by!(Area, name: "unassigned")
          %Area{id: id, name: name, level: level}
      end

    Map.put(room, :area, area)
  end

  def handle_continue(:init, id) do
    room =
      Repo.get!(Room, id)
      |> preload_area()
      |> Trainer.load()
      |> Room.load_exits()
      |> Room.load_items()
      |> Repo.preload(:placed_items)
      |> Shop.load()
      |> Room.load_ability()
      |> MonsterSpawning.load_monsters()
      |> spawn_permanent_npc()

    Logger.metadata(room: "#{room.name} - ##{room.id}")

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")
    PubSub.subscribe("areas:#{room.area_id}")

    send(self(), :perform_maintenance)

    if Shop.shop?(room) do
      send(self(), :restock_shop)
    end

    if room.lair_size && Enum.any?(LairMonster.monster_ids(id)) do
      send(self(), :spawn_lair)
    end

    if room.zone_controller_id > 0 && room.zone_controller_id != room.id do
      send(self(), :spawn_zone_monster)
    end

    if room.ability_id do
      send(self(), :execute_room_ability)
    end

    Process.send_after(self(), :save, 2000)

    send(self(), :cleanup)

    {:noreply, room}
  end

  def handle_call({:find_mobile, mobile_name}, _from, room) do
    mobile =
      room.mobiles
      |> Map.values()
      |> Enum.find(&(&1.name == mobile_name))

    {:reply, mobile, room}
  end

  def handle_call({:scry, ref}, _from, room) do
    if target = room.mobiles[ref] do
      Mobile.send_scroll(
        target,
        "<p><span class='dark-magenta'>You feel the presence of another mind.</span></p>"
      )
    end

    {:reply, :ok, room}
  end

  def handle_call({:enqueue_command, mobile_ref, command, arguments}, _from, room) do
    case ApathyDrive.Command.enqueue(room, mobile_ref, command, arguments) do
      %Room{} = room ->
        {:reply, :ok, room}

      {:error, error, %Room{} = room} ->
        {:reply, error, room}
    end
  end

  def handle_call({:set_chat_tab, mobile_ref, tab}, _from, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn _room, mobile ->
        mobile
        |> Ecto.Changeset.change(%{
          chat_tab: tab
        })
        |> Repo.update!()
      end)

    {:reply, :ok, room}
  end

  def handle_call({:lock, direction}, _from, room) do
    room = Room.lock!(room, direction)
    {:reply, room, room}
  end

  def handle_call({:character_connected, %Character{id: id} = character, socket}, _from, room) do
    if existing_character = Room.find_character(room, id) do
      monitor_ref =
        if existing_character.socket != socket do
          Process.demonitor(existing_character.monitor_ref)
          send(existing_character.socket, :go_home)
          Process.monitor(socket)
        else
          Process.monitor(socket)
        end

      room =
        Room.update_mobile(room, existing_character.ref, fn _room, mob ->
          mob
          |> Map.put(:socket, socket)
          |> TimerManager.cancel(:logout)
          |> Map.put(:monitor_ref, monitor_ref)
        end)

      room.mobiles[existing_character.ref]
      |> Mobile.update_prompt(room)
      |> Character.set_attribute_levels()
      |> Character.update_exp_bar()

      Room.update_moblist(room)

      {:reply, room.mobiles[existing_character.ref], room}
    else
      monitor_ref = Process.monitor(socket)

      ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

      character =
        character
        |> Map.put(:monitor_ref, monitor_ref)
        |> Map.put(:ref, ref)
        |> Map.put(:leader, ref)
        |> Map.put(:socket, socket)
        |> Character.load_traits()
        |> Character.load_race()
        |> Character.load_classes()
        |> Character.set_attribute_levels()
        |> Character.update_exp_bar()
        |> Character.load_skills()
        |> Character.load_abilities()
        |> Character.load_items()
        |> Character.set_title()
        |> Character.set_lore()
        |> Character.load_materials()
        |> Character.load_attunements()
        |> TimerManager.send_after(
          {:reduce_evil_points, :timer.seconds(60), {:reduce_evil_points, ref}}
        )
        |> TimerManager.send_after(
          {:heartbeat, ApathyDrive.Regeneration.tick_time(character), {:heartbeat, ref}}
        )

      Mobile.update_prompt(character, room)

      room = put_in(room.mobiles[character.ref], character)

      Gossip.player_sign_in(character.name)

      Directory.add_character(%{
        name: character.name,
        evil_points: character.evil_points,
        room: character.room_id,
        ref: character.ref,
        title: character.title
      })

      Room.update_moblist(room)

      {:reply, character, room}
    end
  end

  def handle_cast({:send_scroll, html}, %Room{} = room) do
    Room.send_scroll(room, html)

    {:noreply, room}
  end

  def handle_cast({:tell_monsters_to_follow, character, destination}, room) do
    room = Room.tell_monsters_to_follow(room, character, destination)
    {:noreply, room}
  end

  def handle_cast({:tell, from_character_name, from_game, to_character_ref, message}, room) do
    room =
      Room.update_mobile(room, to_character_ref, fn _room, character ->
        from_character =
          [from_character_name, from_game]
          |> Enum.reject(&is_nil/1)
          |> Enum.join("@")

        message =
          "<p><span class='red'>#{from_character} tells you:</span> #{Character.sanitize(message)}</p>"

        Character.send_chat(
          character,
          message
        )

        Repo.insert!(%ChannelHistory{
          character_id: character.id,
          character_name: from_character_name,
          game_name: from_game,
          message: message
        })

        Map.put(character, :reply_to, from_character)
      end)

    {:noreply, room}
  end

  def handle_cast({:system, mobile, command}, %Room{} = room) do
    room = Commands.System.execute(room, mobile, command)

    {:noreply, room}
  end

  def handle_cast({:attacker, attacker, target}, room) do
    Commands.Attack.execute(room, attacker, target)
    {:noreply, room}
  end

  def handle_cast({:get_item, mobile, item}, room) do
    {:noreply, Commands.Get.execute(room, mobile, item)}
  end

  def handle_cast({:trigger_remote_action, remote_action_exit, from, opts}, room) do
    room = Commands.RemoteAction.execute(room, remote_action_exit, from, opts)
    {:noreply, room}
  end

  def handle_cast({:mirror_lock, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>"
      )

      {:noreply, Room.lock!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_close, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed!</p>"
      )

      {:noreply, Room.close!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_open, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened!</p>"
      )

      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>"
      )

      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash_fail, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} shudders from an impact, but it holds!</p>"
      )
    end

    {:noreply, room}
  end

  def handle_cast({:mirror_open_fail, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>Someone tries to open the #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])}, but it's locked.</p>"
      )
    end

    {:noreply, room}
  end

  def handle_cast({:look, spirit_id, args}, %Room{} = room) do
    Commands.Look.execute(room, spirit_id, args)

    {:noreply, room}
  end

  def handle_cast({:mobile_movement, _mobile, _message}, room) do
    {:noreply, room}
  end

  def handle_cast({:mobile_entered, %{} = mobile, message}, room) do
    room = Room.mobile_entered(room, mobile, message)

    {:noreply, room}
  end

  def handle_info(:reload, room) do
    room.mobiles
    |> Enum.each(fn
      {_ref, %Character{socket: socket}} ->
        send(socket, :reconnect)

      {_ref, %{}} ->
        :noop
    end)

    {:stop, :reload, room}
  end

  def handle_info({:clear_hate, monster_ref, hated_ref}, room) do
    room =
      Room.update_mobile(room, monster_ref, fn _room, monster ->
        update_in(monster.hate, &Map.delete(&1, hated_ref))
      end)

    {:noreply, room}
  end

  def handle_info({:execute_command, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn room, mobile ->
        if mobile.current_command do
          Logger.info(
            "#{mobile.name} already executing command: #{inspect(mobile.current_command)}"
          )

          if mobile.current_command_error_count > 10 do
            Logger.info("skipping stuck command: #{inspect(mobile.current_command)} ")

            mobile
            |> Map.put(:current_command, nil)
            |> Map.put(:current_command_error_count, 0)
            |> TimerManager.send_after({:execute_command, 10, {:execute_command, mobile_ref}})
          else
            mobile
            |> TimerManager.send_after({:execute_command, 500, {:execute_command, mobile_ref}})
            |> update_in([:current_command_error_count], &(&1 + 1))
          end
        else
          case :queue.out(mobile.commands) do
            {:empty, _commands} ->
              {:noreply, room}

            {{:value, {command, args}}, commands} ->
              room
              |> put_in([:mobiles, mobile_ref, :commands], commands)
              |> put_in([:mobiles, mobile_ref, :current_command], {command, args})
              |> update_in(
                [:mobiles, mobile_ref],
                &TimerManager.send_after(
                  &1,
                  {:execute_command, 0, {:execute_command, mobile_ref}}
                )
              )
              |> ApathyDrive.Command.execute(mobile_ref, command, args)
              |> case do
                %Room{} = room ->
                  Room.update_mobile(room, mobile_ref, fn _room, mobile ->
                    Map.put(mobile, :current_command, nil)
                  end)

                {:error, :too_tired, room} ->
                  room
              end
          end
        end
      end)

    {:noreply, room}
  end

  def handle_info({:change_race, %{character: id, race: race_id}}, room) do
    character =
      room.mobiles
      |> Map.values()
      |> Enum.find(fn
        %Character{id: ^id} = character ->
          character

        _ ->
          nil
      end)

    if character do
      character = Map.put(character, :race_id, race_id)
      room = Room.reload_character(room, character)

      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info(:perform_maintenance, room) do
    room =
      room
      |> Room.delete_items_for_maintenance()
      |> Room.spawn_placed_items()

    Process.send_after(self(), :perform_maintenance, :timer.hours(18))
    {:noreply, room}
  end

  def handle_info({:DOWN, monitor_ref, :process, _pid, _reason}, room) do
    room =
      case Room.find_monitor_ref(room, monitor_ref) do
        %Character{ref: ref} ->
          Room.update_mobile(room, ref, fn _room, character ->
            TimerManager.send_after(character, {:logout, 30_000, {:logout, ref}})
          end)

        nil ->
          room
      end

    {:noreply, room}
  end

  def handle_info(:cleanup, room) do
    Enum.each(room.items, fn
      %Item{delete_at: nil} ->
        :noop

      %Item{delete_at: delete_at, instance_id: id} = item ->
        if DateTime.compare(DateTime.utc_now(), delete_at) == :gt do
          if item.destruct_message do
            Room.send_scroll(room, "<p>#{item.destruct_message}</p>")
          end

          Repo.delete!(%ItemInstance{id: id})
        end
    end)

    Process.send_after(self(), :cleanup, :timer.minutes(1))

    room =
      room
      |> Room.load_items()
      |> Room.dedup_limited_monsters()

    {:noreply, room}
  end

  def handle_info(:restock_shop, room) do
    room = Shop.restock(room)
    {:noreply, room}
  end

  def handle_info({:logout, ref}, room) do
    character = room.mobiles[ref]
    companion = Character.companion(character, room)

    room =
      room
      |> update_in([:mobiles], &Map.delete(&1, ref))
      |> update_in([:mobiles], &Map.delete(&1, companion && companion.ref))

    Gossip.player_sign_out(character.name)

    Directory.remove_character(character.name)

    {:noreply, room}
  end

  def handle_info({:heartbeat, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn room, mobile ->
        Mobile.heartbeat(mobile, room)
      end)

    {:noreply, room}
  end

  def handle_info({:use_light_source, mobile_ref}, room) do
    room = ApathyDrive.Commands.Use.use_light_source(room, mobile_ref)

    {:noreply, room}
  end

  def handle_info({:rest, mobile_ref}, room) do
    Room.update_hp_bar(room, mobile_ref)
    Room.update_mana_bar(room, mobile_ref)

    {:noreply, room}
  end

  def handle_info({:reduce_evil_points, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn _room, character ->
        points = Character.evil_points_to_restore(character)

        character = Character.alter_evil_points(character, -points)

        Directory.add_character(%{
          name: character.name,
          evil_points: character.evil_points,
          room: character.room_id,
          ref: character.ref,
          title: character.title
        })

        TimerManager.send_after(
          character,
          {:reduce_evil_points, :timer.seconds(60), {:reduce_evil_points, character.ref}}
        )
      end)

    {:noreply, room}
  end

  def handle_info({:expire_invite, ref, invitee_ref}, %Room{} = room) do
    room =
      Room.update_mobile(room, ref, fn _room, mobile ->
        update_in(mobile.invitees, &List.delete(&1, invitee_ref))
      end)

    {:noreply, room}
  end

  def handle_info({:delay_execute_script, mobile_ref, script}, room) do
    mobile = room.mobiles[mobile_ref]
    room = put_in(room.mobiles[mobile_ref], Map.put(mobile, :delayed, false))
    {:noreply, ApathyDrive.Script.execute(room, mobile, script)}
  end

  def handle_info(:load_monsters, room) do
    {:noreply, MonsterSpawning.load_monsters(room)}
  end

  def handle_info(:spawn_permanent_npc, room) do
    {:noreply, spawn_permanent_npc(room)}
  end

  def handle_info({:update_area, area}, room) do
    PubSub.unsubscribe("areas:#{room.area_id}")
    room = Room.update_area(room, area)
    PubSub.subscribe("areas:#{area.id}")
    {:noreply, room}
  end

  def handle_info(:save, room) do
    Process.send_after(self(), :save, jitter(:timer.minutes(30)))
    room = Repo.save!(room)
    {:noreply, room}
  end

  def handle_info(:spawn_zone_monster, %Room{zone_controller_id: nil} = room),
    do: {:noreply, room}

  def handle_info(:spawn_zone_monster, %Room{zone_controller_id: id} = room) do
    room =
      ApathyDrive.MonsterSpawning.spawn_zone_monster(
        room,
        LairMonster.monster_ids(id)
      )

    {:noreply, room}
  end

  def handle_info(
        :spawn_lair,
        %{:lair_next_spawn_at => lair_next_spawn_at} = room
      ) do
    :erlang.send_after(5000, self(), :spawn_lair)

    if DateTime.to_unix(DateTime.utc_now(), :second) >= lair_next_spawn_at do
      room =
        room
        |> ApathyDrive.MonsterSpawning.spawn_lair()
        |> Map.put(
          :lair_next_spawn_at,
          DateTime.to_unix(DateTime.utc_now(), :second) + room.lair_frequency * 60
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

  def handle_info(
        {:timeout, _ref, {name, time, [module, function, args]}},
        %Room{timers: timers} = room
      ) do
    jitter = trunc(time / 2) + :rand.uniform(time)

    new_ref = :erlang.start_timer(jitter, self(), {name, time, [module, function, args]})

    timers = Map.put(timers, name, new_ref)

    apply(module, function, args)

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info(
        {:timeout, _ref, {name, [module, function, args]}},
        %Room{timers: timers} = room
      ) do
    apply(module, function, args)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, room) do
    room = Systems.Effect.remove(room, key, fire_after_cast: true, show_expiration_message: true)
    {:noreply, room}
  end

  def handle_info({:remove_effect, item_instance_id, key}, room)
      when is_integer(item_instance_id) do
    room =
      room.items
      |> Enum.reduce(room, fn
        %{instance_id: ^item_instance_id} = item, room ->
          item =
            Systems.Effect.remove(item, key, fire_after_cast: false, show_expiration_message: room)

          location =
            Enum.find_index(
              room.items,
              &(&1.instance_id == item.instance_id)
            )

          update_in(room.items, &List.replace_at(&1, location, item))

        _, room ->
          room
      end)

    room =
      room.mobiles
      |> Map.values()
      |> Enum.reduce(room, fn
        %Character{} = character, room ->
          room =
            Enum.reduce(character.inventory, room, fn item, room ->
              item =
                Systems.Effect.remove(item, key,
                  fire_after_cast: false,
                  show_expiration_message: character
                )

              location =
                Enum.find_index(
                  character.inventory,
                  &(&1.instance_id == item.instance_id)
                )

              update_in(
                room.mobiles[character.ref].inventory,
                &List.replace_at(&1, location, item)
              )
            end)

          Enum.reduce(character.equipment, room, fn item, room ->
            item =
              Systems.Effect.remove(item, key,
                fire_after_cast: false,
                show_expiration_message: character
              )

            location =
              Enum.find_index(
                character.equipment,
                &(&1.instance_id == item.instance_id)
              )

            update_in(
              room.mobiles[character.ref].equipment,
              &List.replace_at(&1, location, item)
            )
          end)

        _mobile, room ->
          room
      end)

    {:noreply, room}
  end

  def handle_info({:remove_effect, ref, key}, room) do
    room =
      Room.update_mobile(room, ref, fn _room, mobile ->
        Systems.Effect.remove(mobile, key, fire_after_cast: true, show_expiration_message: true)
      end)

    Room.update_hp_bar(room, ref)
    Room.update_mana_bar(room, ref)

    {:noreply, room}
  end

  def handle_info({:room_updated, changes}, room) do
    {:noreply, Map.merge(room, changes)}
  end

  def handle_info(:room_deleted, room) do
    room =
      Enum.reduce(room.mobiles, room, fn {ref, _mobile}, updated_room ->
        Room.update_mobile(updated_room, ref, &Mobile.die(&2, &1))
      end)

    {:stop, :normal, room}
  end

  def handle_info(:tick, room) do
    room =
      room
      |> Room.apply_timers()
      |> Room.start_timer()

    {:noreply, room}
  end

  def handle_info({:lt_tick, time, caster_ref, enchantment}, room) do
    room = Enchantment.tick(room, time, caster_ref, enchantment)
    {:noreply, room}
  end

  def handle_info(:start_timer, %Room{} = room) do
    room = Room.start_timer(room)

    {:noreply, room}
  end

  def handle_info({:execute_script, mobile_ref, script}, room) do
    if mobile = room.mobiles[mobile_ref] do
      {:noreply, ApathyDrive.Script.execute_script(room, mobile, script)}
    else
      {:noreply, room}
    end
  end

  def handle_info(:execute_room_ability, room) do
    room =
      Enum.reduce(room.mobiles, room, fn
        {ref, %Character{}}, room ->
          Ability.execute(room, ref, room.ability, [ref])

        _, room ->
          room
      end)

    Process.send_after(self(), :execute_room_ability, :timer.seconds(5))

    {:noreply, room}
  end

  def handle_info({:execute_ability, %{caster: ref, ability: ability, target: target}}, room) do
    if mobile = room.mobiles[ref] do
      if ability.mana > 0 do
        Mobile.send_scroll(mobile, "<p><span class='cyan'>You cast your spell.</span></p>")
      end

      room = Ability.execute(room, mobile.ref, ability, target)
      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info(:reload_abilities, room) do
    room = Room.load_abilities(room)

    {:noreply, room}
  end

  def handle_info({:goo_janitor, name}, room) do
    Room.send_scroll(room, "<p>A <span class='dark-cyan'>janitor</span> arrives.")

    Room.send_scroll(
      room,
      "<p>The <span class='dark-cyan'>janitor</span> mops up the puddle that was #{name}."
    )

    Room.send_scroll(room, "<p>The <span class='dark-cyan'>janitor</span> leaves.")

    {:noreply, room}
  end

  def handle_info({:item_message, instance_id, message}, room) do
    if Enum.find(room.items, &(&1.instance_id == instance_id)) do
      Room.send_scroll(room, message)
    else
      Enum.find(room.mobiles, fn
        {_ref, %Character{} = character} ->
          Enum.find(character.inventory ++ character.equipment, fn item ->
            item.instance_id == instance_id
          end)

        _ ->
          false
      end)
      |> case do
        {_ref, character} ->
          Mobile.send_scroll(character, message)

        _ ->
          :noop
      end
    end

    {:noreply, room}
  end

  def handle_info(message, room) do
    IO.inspect(message)
    {:noreply, room}
  end

  def execute_casting_ability(%{casting: %Ability{} = ability} = mobile, room) do
    if mobile.energy >= mobile.max_energy do
      mobile = Map.put(mobile, :casting, nil)
      room = put_in(room.mobiles[mobile.ref], mobile)
      Ability.execute(room, mobile.ref, %Ability{} = ability, ability.target_list)
    else
      mobile
    end
  end

  def execute_casting_ability(%{casting: {script, target, _energy_req}} = mobile, room) do
    if mobile.energy >= mobile.max_energy do
      mobile = Map.put(mobile, :casting, nil)
      room = put_in(room.mobiles[mobile.ref], mobile)
      script.execute(room, mobile.ref, target)
    else
      mobile
    end
  end

  def execute_casting_ability(%{casting: {:move, room_exit}} = mobile, room) do
    if Party.exhausted(room, mobile) do
      Map.put(mobile, :casting, {:move, room_exit})
    else
      mobile =
        mobile
        |> Map.put(:current_command, nil)
        |> Map.put(:casting, nil)

      room = put_in(room.mobiles[mobile.ref], mobile)

      Commands.Move.execute(room, mobile, room_exit)
    end
  end

  def execute_casting_ability(%{} = mobile, _room), do: mobile

  defp spawn_permanent_npc(room) do
    time = ((95..100 |> Enum.random()) * :timer.minutes(10)) |> div(100)
    Process.send_after(self(), :spawn_permanent_npc, time)
    MonsterSpawning.spawn_permanent_npc(room)
  end

  defp jitter(time) do
    time
    |> :rand.uniform()
    |> Kernel.+(time)
  end
end
