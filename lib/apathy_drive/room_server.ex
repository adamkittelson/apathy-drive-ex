defmodule ApathyDrive.RoomServer do
  use GenServer

  alias ApathyDrive.{
    Ability,
    Area,
    ChannelHistory,
    Character,
    Commands,
    Companion,
    Directory,
    Enchantment,
    Item,
    ItemInstance,
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

  def handle_continue(:init, id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(area: Area.without_map())
      |> Room.load_exits()
      |> Room.load_items()
      |> Repo.preload(:placed_items)
      |> Room.load_skills()
      |> Shop.load()

    Logger.metadata(room: room.name <> "##{room.id}")

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")
    PubSub.subscribe("areas:#{room.area_id}")

    send(self(), :load_monsters)
    send(self(), :spawn_permanent_npc)

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

    Process.send_after(self(), :save, 2000)

    send(self(), :cleanup)

    {:noreply, room}
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
        |> Character.load_race()
        |> Character.load_class()
        |> Character.set_attribute_levels()
        |> Character.update_exp_bar()
        |> Character.load_abilities()
        |> Character.load_items()
        |> Character.set_title()
        |> Mobile.cpr()

      Mobile.update_prompt(character)

      room =
        put_in(room.mobiles[character.ref], character)
        |> Companion.load_for_character(character)

      Gossip.player_sign_in(character.name)

      Directory.add_character(%{
        name: character.name,
        bounty: character.bounty,
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

  def handle_cast({:tell, from_character_name, from_game, to_character_ref, message}, room) do
    room =
      Room.update_mobile(room, to_character_ref, fn character ->
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
          Room.update_mobile(room, ref, fn character ->
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
          if !is_nil(item.dropped_for_character_id) do
            # change the item to be visible to everyone and
            # set it to delete based on the value of the item
            ItemInstance
            |> Repo.get(id)
            |> Ecto.Changeset.change(%{
              dropped_for_character_id: nil,
              delete_at: Timex.shift(DateTime.utc_now(), minutes: Item.cost_in_copper(item))
            })
            |> Repo.update!()
          else
            Repo.delete!(%ItemInstance{id: id})
          end
        end
    end)

    Process.send_after(self(), :cleanup, :timer.hours(1))

    room = Room.load_items(room)
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

  def handle_info({:use_light_source, mobile_ref}, room) do
    room = ApathyDrive.Commands.Use.use_light_source(room, mobile_ref)

    {:noreply, room}
  end

  def handle_info({:rest, mobile_ref}, room) do
    Room.update_hp_bar(room, mobile_ref)
    Room.update_mana_bar(room, mobile_ref)

    {:noreply, room}
  end

  def handle_info({:reduce_bounty, mobile_ref}, room) do
    room =
      Room.update_mobile(room, mobile_ref, fn character ->
        character =
          character
          |> Ecto.Changeset.change(%{bounty: character.bounty - 1})
          |> Repo.update!()

        Directory.add_character(%{
          name: character.name,
          bounty: character.bounty,
          room: character.room_id,
          ref: character.ref,
          title: character.title
        })

        TimerManager.send_after(
          character,
          {:reduce_bounty, :timer.seconds(60), {:reduce_bounty, character.ref}}
        )
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

  def handle_info(:reload_abilities, room) do
    room = Room.load_abilities(room)

    {:noreply, room}
  end

  def handle_info(message, room) do
    IO.inspect(message)
    {:noreply, room}
  end

  def execute_casting_ability(%{casting: %Ability{} = ability} = mobile, room) do
    if ability.energy <= mobile.energy do
      mobile = Map.put(mobile, :casting, nil)
      room = put_in(room.mobiles[mobile.ref], mobile)
      Ability.execute(room, mobile.ref, %Ability{} = ability, ability.target_list)
    else
      mobile
    end
  end

  def execute_casting_ability(%{} = mobile, _room), do: mobile

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
