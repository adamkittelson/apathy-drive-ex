defmodule ApathyDrive.RoomServer do
  use GenServer
  alias ApathyDrive.{Character, Commands, Companion, LairMonster, LairSpawning, Match, Mobile, Monster, MonsterSpawning,
                     PubSub, Repo, Room, RoomSupervisor, RoomUnity, Spell, TimerManager, Ability, Presence}
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

  def greet(room, greeter, query) do
    GenServer.cast(room, {:greet, greeter, query})
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

  def execute_command(room, spirit_id, command, arguments) do
    GenServer.cast(room, {:execute_command, spirit_id, command, arguments})
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

  def destroy_item(room, item) do
    GenServer.call(room, {:destroy_item, item})
  end

  def character_connected(room, character, socket) do
    GenServer.call(room, {:character_connected, character, socket})
  end

  def init(id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(:room_unity)
      |> Repo.preload(:area)
      |> Room.load_items

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

    send(self, :load_monsters)
    send(self, :spawn_permanent_npc)

    if room.lair_size && Enum.any?(LairMonster.monster_ids(id)) do
      send(self, :spawn_monsters)
    end

    Process.send_after(self(), :save, 2000)

    # room =
    #   room
    #   |> TimerManager.send_after({:report_essence, Application.get_env(:apathy_drive, :initial_essence_delay), :report_essence})
    #   |> TimerManager.send_after({:update_essence, Room.essence_update_interval(room), :update_essence})

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

  def handle_call({:character_connected, %Character{id: id} = character, socket}, _from, room) do
    if mobile = Room.find_character(room, id) do

      monitor_ref =
        if mobile.socket != socket do
          Process.demonitor(character.monitor_ref)
          send(character.socket, :go_home)
          Process.monitor(socket)
        else
          Process.monitor(socket)
        end

      room =
        Room.update_mobile(room, mobile.ref, fn(mob) ->
          mob = Map.put(mob, :socket, socket)
          put_in(character.monitor_ref, monitor_ref)
        end)

      room.mobiles[mobile.ref]
      |> Mobile.update_prompt

      case Presence.track(socket, "spirits:online", character.id, %{name: character.name}) do
        {:ok, _} ->
          :ok
        {:error, {:already_tracked, _pid, _topic, _key}} ->
          :ok
      end

      {:reply, character, room}
    else
      monitor_ref = Process.monitor(socket)

      ref = make_ref()

      character =
        character
        |> Map.put(:monitor_ref, monitor_ref)
        |> Map.put(:ref, ref)
        |> Map.put(:leader, ref)
        |> Character.load_race
        |> Character.load_class
        |> Character.load_spells
        |> Character.load_items
        |> Map.put(:socket, socket)
        |> TimerManager.send_after({:regen, 1_000, {:regen, ref}})

      Mobile.update_prompt(character)

      room =
        put_in(room.mobiles[character.ref], character)
        |> Companion.load_for_character(character)

      case Presence.track(socket, "spirits:online", character.id, %{name: character.name}) do
        {:ok, _} ->
          :ok
        {:error, {:already_tracked, _pid, _topic, _key}} ->
          :ok
      end

      {:reply, character, room}
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

  def handle_cast({:greet, greeter, query}, %Room{} = room) do
    Commands.Greet.execute(room, greeter, query)
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
      Room.send_scroll(room, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>")
      {:noreply, Room.lock!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_close, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(room, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed!</p>")
      {:noreply, Room.close!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_open, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(room, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened!</p>")
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(room, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>")
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash_fail, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(room, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} shudders from an impact, but it holds!</p>")
    end
    {:noreply, room}
  end

  def handle_cast({:mirror_open_fail, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(room, "<p>Someone tries to open the #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])}, but it's locked.</p>")
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

  def handle_info({:DOWN, monitor_ref, :process, _pid, _reason}, room) do
    room =
      case Room.find_monitor_ref(room, monitor_ref) do
        %Character{ref: ref} ->
          update_in(room.mobiles, &Map.delete(&1, ref))
        nil ->
          room
      end

    {:noreply, room}
  end

  def handle_info({:unify, ref}, room) do
    case Room.get_mobile(room, ref) do
      %{spirit: nil, unities: []} ->
        :noop
      %{spirit: nil, experience: essence, unities: unities} ->
        Enum.each(unities, fn(unity) ->
          ApathyDrive.Unity.contribute(unity, essence)
        end)
      %{spirit: %Spirit{experience: essence, class: %{unities: unities}}} ->
        Enum.each(unities, fn(unity) ->
          ApathyDrive.Unity.contribute(unity, essence)
        end)
      _ ->
        :noop
    end

    room = Room.update_mobile(room, ref, fn mobile ->
      TimerManager.send_after(mobile, {:unify, 60_000, {:unify, ref}})
    end)

    {:noreply, room}
  end

  def handle_info({:update_unity_essence, unity, essence}, room) do
    room =
      Enum.reduce(room.mobiles, room, fn {ref, _mobile}, updated_room ->
        Room.update_mobile(updated_room, ref, fn mobile ->
          put_in(mobile.unity_essences[unity], essence)
        end)
      end)

    {:noreply, room}
  end

  def handle_info({:regen, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn mobile ->
        Mobile.regenerate_hp_and_mana(mobile, room)
      end)

    {:noreply, room}
  end

  def handle_info({:heartbeat, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn mobile ->
        Mobile.heartbeat(mobile, room)
      end)

    {:noreply, room}
  end

  def handle_info({:auto_move, ref}, room) do
    if mobile = Room.get_mobile(room, ref) do
      if should_move?(room, mobile) do
        exits =
          case room.exits do
            nil ->
              []
            _exits ->
              case mobile.unities do
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
          end

        if Enum.any?(exits) do
          room_exit = Enum.random(exits)
          {:noreply, Commands.Move.execute(room, mobile, room_exit)}
        else
          {:noreply, Room.move_after(room, ref)}
        end
      else
        {:noreply, Room.move_after(room, ref)}
      end
    else
      {:noreply, room}
    end
  end


  def handle_info({:think, ref}, room) do
    room = ApathyDrive.AI.think(room, ref)

    {:noreply, room}
  end

  def handle_info({:expire_invite, ref, invitee_ref}, %Room{} = room) do
    room =
      Room.update_mobile(room, ref, fn mobile ->
        update_in(mobile.invitees, &List.delete(&1, invitee_ref))
      end)

    {:noreply, room}
  end

  def handle_info({:execute_auto_attack, ref}, %Room{} = room) do
    room =
      Room.update_mobile(room, ref, fn
        %{} = mobile ->
          attack = Mobile.attack_spell(mobile)
          if target_ref = Mobile.auto_attack_target(mobile, room, attack) do
            mobile = TimerManager.send_after(mobile, {:auto_attack_timer, Mobile.attack_interval(mobile), {:execute_auto_attack, ref}})
            room = put_in(room.mobiles[mobile.ref], mobile)

            Spell.execute(room, mobile.ref, attack, [target_ref])
          else
            case mobile do
              %Character{} = character ->
                Mobile.send_scroll(character, "<p><span class='dark-yellow'>*Combat Off*</span></p>")
                Map.put(character, :attack_target, nil)
              mobile ->
                mobile
              end
          end
      end)
    {:noreply, room}
  end

  def handle_info({:apply_periodic_effects, ref}, room) do

    if mobile = Room.get_mobile(room, ref) do
      room =
        mobile.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "heal")))
        |> Enum.reduce(room, fn(%{"heal" => heal, "effect_message" => message}, updated_room) ->
             ability = %{
               "kind" => "heal",
               "ignores_global_cooldown" => true,
               "flags" => [],
               "instant_effects" => %{"heal" => heal},
               "cast_message"    => %{"user" => message}
             }

             Ability.execute(updated_room, ref, ability, [ref])
           end)

      room =
        mobile.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "heal_mana")))
        |> Enum.reduce(room, fn(%{"heal_mana" => heal, "effect_message" => message}, updated_room) ->
             ability = %{
               "kind" => "heal",
               "ignores_global_cooldown" => true,
               "flags" => [],
               "instant_effects" => %{"heal_mana" => heal},
               "cast_message"    => %{"user" => message}
             }

             Ability.execute(updated_room, ref, ability, [ref])
           end)

      room =
        mobile.effects
        |> Map.values
        |> Enum.filter(&(Map.has_key?(&1, "damage")))
        |> Enum.reduce(room, fn(%{"damage" => damage, "effect_message" => message}, updated_room) ->
             ability = %{
               "kind" => "attack",
               "ignores_global_cooldown" => true,
               "flags" => [],
               "instant_effects" => %{"damage" => damage},
               "cast_message"    => %{"user" => message}
             }

             Ability.execute(updated_room, ref, ability, [ref])
           end)

      room = TimerManager.send_after(room, {:periodic_effects, 1_000, {:apply_periodic_effects, ref}})

      {:noreply, room}
    else
      {:noreply, room}
    end
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
    {:noreply, MonsterSpawning.spawn_permanent_npc(room)}
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
        |> ApathyDrive.MonsterSpawning.spawn_lair
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

  def handle_info(:update_essence, %Room{} = room) do
    room =
      room
      |> Room.update_essence
      |> TimerManager.send_after({:update_essence, Room.essence_update_interval(room), :update_essence})

    {:noreply, room}
  end

  def handle_info(:report_essence, room) do
    room = Room.report_essence(room)

    {:noreply, room}
  end

  def handle_info({:essence_report, report}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, report.room_id)

    room = put_in(room.room_unity.exits[mirror_exit["direction"]], %{"essences" => report.essences, "area" => report.area, "controlled_by" => report.controlled_by})

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

  def handle_info({:remove_effect, ref, key}, room) do
    room = Room.update_mobile(room, ref, fn mobile ->
      Systems.Effect.remove(mobile, key, fire_after_cast: true, show_expiration_message: true)
    end)
    {:noreply, room}
  end

  def handle_info({:room_updated, changes}, room) do
    {:noreply, Map.merge(room, changes)}
  end

  def handle_info(:room_deleted, room) do
    room =
      Enum.reduce(room.mobiles, room, fn {ref, _mobile}, updated_room ->
        Room.update_mobile(room, ref, &ApathyDrive.Death.kill(updated_room, &1.ref))
      end)
    {:stop, :normal, room}
  end

  def handle_info(:tick, room) do
    room =
      room
      |> Room.apply_timers
      |> Room.start_timer

    {:noreply, room}
  end

  def handle_info(:start_timer, %Room{} = room) do
    room = Room.start_timer(room)

    {:noreply, room}
  end

  def handle_info({:timer_cast_ability, %{caster: ref, ability: ability, timer: time, target: target}}, room) do
    if mobile = room.mobiles[ref] do
      Monster.send_scroll(mobile, "<p><span class='dark-yellow'>You cast your spell.</span></p>")

      ability = case ability do
        %{"global_cooldown" => nil} ->
          ability
          |> Map.delete("global_cooldown")
          |> Map.put("ignores_global_cooldown", true)
        %{"global_cooldown" => cooldown} ->
          if cooldown > time do
            Map.put(ability, "global_cooldown", cooldown - time)
          else
            ability
            |> Map.delete("global_cooldown")
            |> Map.put("ignores_global_cooldown", true)
          end
        _ ->
          ability
      end

      send(self, {:execute_ability, %{caster: ref, ability: Map.delete(ability, "cast_time"), target: target}})

      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info({:execute_script, mobile_ref, script}, room) do
    if mobile = room.mobiles[mobile_ref] do
      {:noreply, ApathyDrive.Script.execute(room, mobile, script)}
    else
      {:noreply, room}
    end
  end

  def handle_info({:execute_spell, %{caster: ref, spell: spell, target: target}}, room) do
    if mobile = room.mobiles[ref] do
      room = Spell.execute(room, mobile.ref, spell, target)
      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info(message, room) do
    IO.inspect(message)
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

  defp should_move?(%Room{}, %{movement: "stationary"}), do: false
  defp should_move?(%Room{} = room, %{spirit: nil} = mobile) do
    cond do
      # at least 80% health and no enemies present, go find something to kill
      ((mobile.hp / mobile.max_hp) >= 0.8) and !Enum.any?(Room.local_hated_targets(room, mobile)) ->
        true
      # 30% or less health and enemies present, run away!
      ((mobile.hp / mobile.max_hp) <= 0.3) and Enum.any?(Room.local_hated_targets(room, mobile)) ->
        true
      true ->
        false
    end
  end
  defp should_move?(%Room{}, %{}), do: false

end
