defmodule ApathyDrive.RoomServer do
  use GenServer

  alias ApathyDrive.{
    Ability,
    Character,
    Commands,
    Companion,
    Directory,
    Enchantment,
    Energy,
    LairMonster,
    Mobile,
    MonsterSpawning,
    PubSub,
    Repo,
    Room,
    RoomSupervisor,
    Shop,
    TimerManager
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

  def character_connected(room, character, socket) do
    GenServer.call(room, {:character_connected, character, socket})
  end

  def tell(room, from_character_name, to_character_name, message) do
    GenServer.cast(room, {:tell, from_character_name, to_character_name, message})
  end

  def init(id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(:area)
      |> Room.load_items()
      |> Room.load_reputations()
      |> Room.load_skills()
      |> Shop.load()

    Logger.metadata(room: room.name <> "##{room.id}")

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")
    PubSub.subscribe("areas:#{room.area_id}")

    send(self(), :load_monsters)
    send(self(), :spawn_permanent_npc)

    if Shop.shop?(room) do
      send(self(), :restock_shop)
    end

    if room.lair_size && Enum.any?(LairMonster.monster_ids(id)) do
      send(self(), :spawn_monsters)
    end

    Process.send_after(self(), :save, 2000)

    {:ok, room}
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
        Room.update_mobile(room, existing_character.ref, fn mob ->
          mob
          |> Map.put(:socket, socket)
          |> TimerManager.cancel(:logout)
          |> Map.put(:monitor_ref, monitor_ref)
        end)

      room.mobiles[existing_character.ref]
      |> Mobile.update_prompt()

      {:reply, room.mobiles[existing_character.ref], room}
    else
      monitor_ref = Process.monitor(socket)

      ref = make_ref()

      character =
        character
        |> Map.put(:monitor_ref, monitor_ref)
        |> Map.put(:ref, ref)
        |> Map.put(:leader, ref)
        |> Character.load_race()
        |> Character.load_class()
        |> Character.load_reputations()
        |> Character.set_attribute_levels()
        |> Character.load_abilities()
        |> Character.load_items()
        |> Character.set_title()
        |> Map.put(:socket, socket)
        |> Mobile.cpr()

      Mobile.update_prompt(character)

      room =
        put_in(room.mobiles[character.ref], character)
        |> Companion.load_for_character(character)

      Gossip.player_sign_in(character.name)

      Directory.add_character(%{
        name: character.name,
        room: character.room_id,
        ref: character.ref,
        title: character.title
      })

      {:reply, character, room}
    end
  end

  def handle_cast({:send_scroll, html}, %Room{} = room) do
    Room.send_scroll(room, html)

    {:noreply, room}
  end

  def handle_cast({:tell, from_character_name, to_character_ref, message}, room) do
    room =
      Room.update_mobile(room, to_character_ref, fn character ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>#{from_character_name} tells you:</span> #{
            Character.sanitize(message)
          }"
        )

        Map.put(character, :reply_to, from_character_name)
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
        "<p>The #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        } just locked!</p>"
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
        "<p>The #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        } just closed!</p>"
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
        "<p>The #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        } just opened!</p>"
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
        "<p>The #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        } just flew open!</p>"
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
        "<p>The #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        } shudders from an impact, but it holds!</p>"
      )
    end

    {:noreply, room}
  end

  def handle_cast({:mirror_open_fail, mirror_room_id, room_exit}, %Room{} = room) do
    mirror_exit = Room.mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      Room.send_scroll(
        room,
        "<p>Someone tries to open the #{String.downcase(mirror_exit["kind"])} #{
          ApathyDrive.Exit.direction_description(mirror_exit["direction"])
        }, but it's locked.</p>"
      )
    end

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
          Room.update_mobile(room, ref, fn character ->
            TimerManager.send_after(character, {:logout, 30_000, {:logout, ref}})
          end)

        nil ->
          room
      end

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
      Room.update_mobile(room, mobile_ref, fn mobile ->
        Mobile.heartbeat(mobile, room)
      end)

    {:noreply, room}
  end

  def handle_info({:regenerate_energy, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn mobile ->
        mobile
        |> Energy.regenerate()
        |> execute_casting_ability(room)
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
              exits_in_area(room)
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

  def handle_info({:expire_invite, ref, invitee_ref}, %Room{} = room) do
    room =
      Room.update_mobile(room, ref, fn mobile ->
        update_in(mobile.invitees, &List.delete(&1, invitee_ref))
      end)

    {:noreply, room}
  end

  def handle_info({:execute_auto_attack, ref}, %Room{} = room) do
    room =
      Room.update_mobile(room, ref, fn %{} = mobile ->
        attack = Mobile.attack_ability(mobile)
        # max 5 auto attacks per "round"
        time = div(Mobile.round_length_in_ms(mobile), 5)

        if mobile.energy >= attack.energy and !mobile.casting do
          if target_ref = Mobile.auto_attack_target(mobile, room, attack) do
            if TimerManager.time_remaining(mobile, :casting) == 0 do
              mobile =
                TimerManager.send_after(
                  mobile,
                  {:auto_attack_timer, time, {:execute_auto_attack, ref}}
                )

              room = put_in(room.mobiles[mobile.ref], mobile)

              Ability.execute(room, mobile.ref, attack, [target_ref])
            else
              TimerManager.send_after(
                mobile,
                {:auto_attack_timer, time, {:execute_auto_attack, ref}}
              )
            end
          else
            case mobile do
              %Character{attack_target: target} = character when is_reference(target) ->
                Mobile.send_scroll(
                  character,
                  "<p><span class='dark-yellow'>*Combat Off*</span></p>"
                )

                Map.put(character, :attack_target, nil)

              mobile ->
                mobile
            end
          end
        else
          TimerManager.send_after(
            mobile,
            {:auto_attack_timer, time, {:execute_auto_attack, ref}}
          )
        end
      end)

    {:noreply, room}
  end

  def handle_info({:periodic_effects, target_ref, effect_ref}, room) do
    room =
      Room.update_mobile(room, target_ref, fn mobile ->
        case Systems.Effect.find_by_ref(mobile, effect_ref) do
          {key, %{"Interval" => interval} = effect} ->
            effect = Map.take(effect, ["Heal", "Damage"])

            room
            |> Room.update_mobile(target_ref, fn mobile ->
              mobile
              |> put_in(
                [Access.key!(:effects), key, "NextEffectAt"],
                System.monotonic_time(:milliseconds) + interval
              )
              |> Systems.Effect.schedule_next_periodic_effect()
            end)
            |> Ability.execute(
              target_ref,
              %Ability{traits: effect, ignores_round_cooldown?: true},
              [target_ref]
            )

          nil ->
            room
        end
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
    {:noreply, MonsterSpawning.spawn_permanent_npc(room)}
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

  def handle_info(
        :spawn_monsters,
        %{:lair_next_spawn_at => lair_next_spawn_at} = room
      ) do
    :erlang.send_after(5000, self(), :spawn_monsters)

    if DateTime.to_unix(DateTime.utc_now(), :seconds) >= lair_next_spawn_at do
      room =
        room
        |> ApathyDrive.MonsterSpawning.spawn_lair()
        |> Map.put(
          :lair_next_spawn_at,
          DateTime.to_unix(DateTime.utc_now(), :seconds) + room.lair_frequency * 60
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

  def handle_info({:remove_effect, ref, key}, room) do
    room =
      Room.update_mobile(room, ref, fn mobile ->
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
        Room.update_mobile(room, ref, &Mobile.die(&1, updated_room))
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
      {:noreply, ApathyDrive.Script.execute(room, mobile, script)}
    else
      {:noreply, room}
    end
  end

  def handle_info({:execute_ability, %{caster: ref, ability: ability, target: target}}, room) do
    if mobile = room.mobiles[ref] do
      Mobile.send_scroll(mobile, "<p><span class='cyan'>You cast your spell.</span></p>")
      room = Ability.execute(room, mobile.ref, ability, target)
      {:noreply, room}
    else
      {:noreply, room}
    end
  end

  def handle_info(:reload_reputations, room) do
    room = Room.load_reputations(room)

    {:noreply, room}
  end

  def handle_info(:reload_abilities, room) do
    room = Room.load_abilities(room)

    {:noreply, room}
  end

  def handle_info(message, room) do
    IO.inspect(message)
    {:noreply, room}
  end

  defp execute_casting_ability(%{casting: %Ability{} = ability} = mobile, room) do
    if ability.energy <= mobile.energy do
      mobile = Map.put(mobile, :casting, nil)
      room = put_in(room.mobiles[mobile.ref], mobile)
      Ability.execute(room, mobile.ref, %Ability{} = ability, ability.target_list)
    else
      mobile
    end
  end

  defp execute_casting_ability(%{} = mobile, _room), do: mobile

  defp jitter(time) do
    time
    |> :rand.uniform()
    |> Kernel.+(time)
  end

  defp exits_in_area(%Room{exits: exits} = room) do
    Enum.filter(exits, fn %{"direction" => direction} = room_exit ->
      room.room_unity.exits[direction] &&
        room.room_unity.exits[direction]["area"] == room.area.name && passable?(room, room_exit)
    end)
  end

  defp passable?(room, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"],
    do: ApathyDrive.Doors.open?(room, room_exit)

  defp passable?(_room, %{"kind" => kind}) when kind in ["Normal", "Action", "Trap", "Cast"],
    do: true

  defp passable?(_room, _room_exit), do: false

  defp should_move?(%Room{}, %{movement: "stationary"}), do: false

  defp should_move?(%Room{} = room, %{spirit: nil} = mobile) do
    cond do
      # at least 80% health and no enemies present, go find something to kill
      mobile.hp / mobile.max_hp >= 0.8 and !Enum.any?(Room.local_hated_targets(room, mobile)) ->
        true

      # 30% or less health and enemies present, run away!
      mobile.hp / mobile.max_hp <= 0.3 and Enum.any?(Room.local_hated_targets(room, mobile)) ->
        true

      true ->
        false
    end
  end

  defp should_move?(%Room{}, %{}), do: false
end
