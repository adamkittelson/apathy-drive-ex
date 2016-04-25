defmodule Room do
  require Logger
  use ApathyDrive.Web, :model
  use GenServer
  use Timex
  import Systems.Text
  alias ApathyDrive.{Commands, PubSub, Mobile, TimerManager, Ability, Match, RoomUnity, RoomSupervisor}

  schema "rooms" do
    field :name,                  :string
    field :keywords,              {:array, :string}
    field :description,           :string
    field :effects,               :any, virtual: true, default: %{}
    field :light,                 :integer
    field :item_descriptions,     ApathyDrive.JSONB, default: %{"hidden" => %{}, "visible" => %{}}
    field :lair_size,             :integer
    field :lair_frequency,        :integer, default: 5
    field :lair_next_spawn_at,    :any, virtual: true, default: 0
    field :exits,                 ApathyDrive.JSONB, default: []
    field :commands,              ApathyDrive.JSONB, default: %{}
    field :legacy_id,             :string
    field :timers,                :any, virtual: true, default: %{}
    field :room_ability,          :any, virtual: true
    field :items,                 ApathyDrive.JSONB, default: []
    field :last_effect_key,       :any, virtual: true, default: 0
    field :also_here,             :map, virtual: true, default: %{}
    field :area,                  :string

    timestamps

    has_one    :room_unity, RoomUnity
    has_many   :mobiles, Mobile
    belongs_to :ability, Ability
    has_many   :lairs, ApathyDrive.LairMonster
    has_many   :lair_monsters, through: [:lairs, :monster_template]
  end

  def init(id) do
    room =
      Repo.get!(Room, id)
      |> Repo.preload(:room_unity)
      |> Repo.preload(:lair_monsters)

    unless room.room_unity do
      room_unity =
        room
        |> build_assoc(:room_unity, essences: %{"good" => 0, "evil" => 0, "default" => default_essence(room)})
        |> Repo.save!

      room = %{room | room_unity: room_unity}
    end

    PubSub.subscribe("rooms")
    PubSub.subscribe("rooms:#{room.id}")

    room.exits
    |> Enum.each(fn(room_exit) ->
         PubSub.subscribe("rooms:#{room_exit["destination"]}:adjacent")
       end)

    load_present_mobiles(self())

    if room.lair_size && Enum.any?(ApathyDrive.LairMonster.monsters_template_ids(id)) do
      send(self, :spawn_monsters)
    end

    if room.ability_id do
      PubSub.subscribe("rooms:abilities")

      room =
        room
        |> Map.put(:room_ability, ApathyDrive.Repo.get(Ability, room.ability_id).properties)
        |> TimerManager.send_every({:execute_room_ability, 5_000, :execute_room_ability})
    end

    room =
      room
      |> TimerManager.send_every({:spread_essence, 60_000, :spread_essence})

    if Application.get_env(:apathy_drive, :quick_load), do: Process.send_after(self(), :spread_essence, 1000)
    Process.send_after(self(), :save, 2000)

    {:ok, room}
  end

  def essence_reaction(%Room{room_unity: %RoomUnity{essences: %{"good" => good, "evil" => evil}}} = room) when good > 0 and evil > 0 do
    good_to_remove =
      good
      |> div(50)
      |> max(1)

    evil_to_remove =
      evil
      |> div(50)
      |> max(1)

    update_in(room.room_unity.essences, &(&1 |> Map.put("good", good - good_to_remove) |> Map.put("evil", evil - evil_to_remove)))
  end
  def essence_reaction(%Room{} = room), do: room

  def controlled_by(%Room{} = room) do
    room.room_unity.controlled_by
  end

  def update_controlled_by(%Room{room_unity: %RoomUnity{essences: essences}} = room) do
    controlled_by = room.room_unity.controlled_by

    highest_essence =
      essences
      |> Map.keys
      |> Enum.sort_by(&Map.get(essences, &1, 0), &>=/2)
      |> List.first

    new_controlled_by =
      cond do
        highest_essence == "good" and (essences["good"] * 0.9) > essences["evil"] ->
          "good"
        highest_essence == "evil" and (essences["evil"] * 0.9) > essences["good"] ->
          "evil"
        true ->
          nil
      end

    if controlled_by != new_controlled_by do
      put_in(room.room_unity.controlled_by, new_controlled_by)
      |> save!
    else
      room
    end
  end

  def default_essence(%Room{lair_monsters: []}), do: 0
  def default_essence(%Room{lair_monsters: lair_monsters}) do
    lair_monsters
    |> Enum.map(fn(mt) ->
         ApathyDrive.Level.exp_at_level(mt.level)
       end)
    |> Enum.sum
    |> div(length(lair_monsters))
  end

  def save!(%Room{room_unity: room_unity} = room) do
    room
    |> Map.put(:room_unity, Repo.save!(room_unity))
    |> Repo.save!
  end

  def changeset(%Room{} = room, params \\ :empty) do
    room
    |> cast(params, ~w(name description exits), ~w(light item_descriptions lair_size lair_frequency commands legacy_id))
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def find_mobile_in_room(%Room{also_here: mobiles}, mobile, query) do
    mobile =
      mobiles
      |> Map.values
      |> Enum.find(&(&1.pid == mobile))

    mobiles
    |> Map.values
    |> Enum.reject(&(&1 == mobile))
    |> List.insert_at(-1, mobile)
    |> Match.one(:name_contains, query)
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(mt) ->
         "#{mt.name} - #{mt.id}"
       end)
  end

  def start_room_id do
    ApathyDrive.Config.get(:start_room)
  end

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

  def all do
    PubSub.subscribers("rooms")
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

  def add_essence_from_mobile(room, mobile, unity, essence) do
    GenServer.cast(room, {:add_essence_from_mobile, mobile, unity, essence})
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

  def notify_presence(room, data) do
    GenServer.cast(room, {:mobile_present, data})
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

  def destroy_item(room, item) do
    GenServer.call(room, {:destroy_item, item})
  end

  def find_item(%Room{items: items, item_descriptions: item_descriptions}, item) do
    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Match.one(:keyword_starts_with, item)

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
        %{description: item_descriptions["visible"][visible_item.name]}
      hidden_item ->
        %{description: item_descriptions["hidden"][hidden_item.name]}
      actual_item ->
        actual_item.item
      true ->
        nil
    end
  end

  def get_exit(room, direction) do
    room
    |> Map.get(:exits)
    |> Enum.find(&(&1["direction"] == direction(direction)))
  end

  def mirror_exit(%Room{} = room, destination_id) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(%{"destination" => destination, "kind" => kind}) ->
         destination == destination_id and kind != "RemoteAction"
       end)
  end

  def command_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(ex) ->
         ex["kind"] == "Command" and Enum.member?(ex["commands"], string)
       end)
  end

  def remote_action_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(ex) ->
         ex["kind"] == "RemoteAction" and Enum.member?(ex["commands"], string)
       end)
  end

  def command(%Room{} = room, string) do
    command =
      room
      |> Map.get(:commands, %{})
      |> Map.keys
      |> Enum.find(fn(command) ->
           String.downcase(command) == String.downcase(string)
         end)

    if command do
      room.commands[command]
    end
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

  def unlocked?(%Room{effects: effects}, direction) do
    effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :unlocked)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :unlocked)
       end)
    |> Enum.member?(direction)
  end

  def temporarily_open?(%Room{} = room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :open)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :open)
       end)
    |> Enum.member?(direction)
  end

  def searched?(room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :searched)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :searched)
       end)
    |> Enum.member?(direction)
  end

  def exit_direction("up"),      do: "upwards"
  def exit_direction("down"),    do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction(nil),       do: "nowhere"
  def enter_direction("up"),      do: "above"
  def enter_direction("down"),    do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def spawned_monster_count(room_id) do
    ApathyDrive.Mobile
    |> where(spawned_at: ^room_id)
    |> select([m], count(m.id))
    |> Repo.one
  end

  def mobiles_to_load(room_id) do
    ApathyDrive.Mobile
    |> where(room_id: ^room_id)
    |> select([m], m.id)
    |> Repo.all
  end

  # Value functions
  def mobiles(%{room_id: room_id, mobile: pid}) do
    PubSub.subscribers("rooms:#{room_id}:mobiles", [pid])
  end

  def mobiles(%Mobile{room_id: room_id}) do
    PubSub.subscribers("rooms:#{room_id}:mobiles")
  end

  def mobiles(%Room{} = room) do
    PubSub.subscribers("rooms:#{room.id}:mobiles")
  end

  def send_scroll(id, html) when is_integer(id) do
    ApathyDrive.PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, html}
  end
  def send_scroll(%Room{id: id}, html) do
    ApathyDrive.PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, html}
  end

  def open!(%Room{} = room, direction) do
    if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      Systems.Effect.add(room, %{open: direction}, open_duration)
    else
      exits = room.exits
              |> Enum.map(fn(room_exit) ->
                   if room_exit["direction"] == direction do
                     Map.put(room_exit, "open", true)
                   else
                     room_exit
                   end
                 end)
      Map.put(room, :exits, exits)
    end
  end

  def close!(%Room{effects: effects} = room, direction) do
    room = effects
           |> Map.keys
           |> Enum.filter(fn(key) ->
                effects[key][:open] == direction
              end)
           |> Enum.reduce(room, fn(room, key) ->
                Systems.Effect.remove(room, key, show_expiration_message: true)
              end)

    exits = room.exits
            |> Enum.map(fn(room_exit) ->
                 if room_exit["direction"] == direction do
                   Map.delete(room_exit, "open")
                 else
                   room_exit
                 end
               end)

    room = Map.put(room, :exits, exits)

    unlock!(room, direction)
  end

  def lock!(%Room{effects: effects} = room, direction) do
    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:unlocked] == direction
       end)
    |> Enum.reduce(room, fn(key, room) ->
         Systems.Effect.remove(room, key, show_expiration_message: true)
       end)
  end

  def get_direction_by_destination(%Room{exits: exits}, destination_id) do
    exit_to_destination = exits
                          |> Enum.find(fn(room_exit) ->
                               room_exit["destination"] == destination_id
                             end)
    exit_to_destination && exit_to_destination["direction"]
  end

  defp unlock!(%Room{} = room, direction) do
    unlock_duration = if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      open_duration
    else
      10#300
    end

    Systems.Effect.add(room, %{unlocked: direction}, unlock_duration)
    # todo: tell players in the room when it re-locks
    #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
  end

  def direction(direction) do
    case direction do
      "n" ->
        "north"
      "ne" ->
        "northeast"
      "e" ->
        "east"
      "se" ->
        "southeast"
      "s" ->
        "south"
      "sw" ->
        "southwest"
      "w" ->
        "west"
      "nw" ->
        "northwest"
      "u" ->
        "up"
      "d" ->
        "down"
      direction ->
        direction
    end
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
          |> save!
        {:reply, {:ok, actual_item.item}, room}
      true ->
        {:reply, :not_found, room}
    end
  end

  def handle_call({:lock, direction}, _from, room) do
    room = lock!(room, direction)
    {:reply, room, room}
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

  def handle_cast({:add_essence_from_mobile, mobile, unity, essence}, %Room{} = room) when unity in ["good", "evil"] do
    room = update_in(room.room_unity.essences[unity], &(&1 + essence))

    essence_to_send_back = div(room.room_unity.essences[unity], 100)

    room =
      update_in(room.room_unity.essences[unity], &(&1 - essence_to_send_back))
      |> update_controlled_by

    Mobile.add_experience(mobile, essence_to_send_back)

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

  def handle_cast({:trigger_remote_action, remote_action_exit, from}, room) do
    room = Commands.RemoteAction.execute(room, remote_action_exit, from)
    {:noreply, room}
  end

  def handle_cast({:search, direction}, room) do
    room = Systems.Effect.add(room, %{searched: direction}, 300)
    {:noreply, room}
  end

  def handle_cast({:mirror_lock, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just locked!</p>"}
      {:noreply, Room.lock!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_close, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just closed!</p>"}
      {:noreply, Room.close!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_open, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just opened!</p>"}
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = mirror_exit(room, mirror_room_id)

    if mirror_exit["kind"] == room_exit["kind"] do
      PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, "<p>The #{String.downcase(mirror_exit["kind"])} #{ApathyDrive.Exit.direction_description(mirror_exit["direction"])} just flew open!</p>"}
      {:noreply, Room.open!(room, mirror_exit["direction"])}
    else
      {:noreply, room}
    end
  end

  def handle_cast({:mirror_bash_fail, mirror_room_id, room_exit}, %Room{id: id} = room) do
    mirror_exit = mirror_exit(room, mirror_room_id)

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

  def handle_cast({:execute_ability, mobile, %{"kind" => kind} = ability, query}, %Room{also_here: mobiles} = room) when kind in ["attack", "curse"] do
    mobile =
      mobiles
      |> Map.values
      |> Enum.find(&(&1.pid == mobile))

   target =
     mobiles
     |> Map.values
     |> Enum.reject(&(&1 == mobile))
     |> Match.one(:name_contains, query)

    send(mobile.pid, {:execute_ability, ability, List.wrap(target && target.pid)})

    {:noreply, room}
  end

  def handle_cast({:mobile_present, %{intruder: mobile, look_name: look_name, name: name} = data}, room) do
    ApathyDrive.PubSub.broadcast_from! mobile, "rooms:#{room.id}:mobiles", {:monster_present, data}
    {:noreply, put_in(room.also_here[mobile], %{look_name: look_name, name: name, keywords: String.split(name), pid: mobile})}
  end

  def handle_cast({:look, mobile, args}, %Room{} = room) do
    present_mobiles = mobiles(room)

    mobiles =
      room.also_here
      |> Enum.reduce(%{}, fn({pid, data}, also_here) ->
           if pid in present_mobiles, do: Map.put(also_here, pid, data), else: also_here
         end)

    room = Map.put(room, :also_here, mobiles)

    Commands.Look.execute(room, mobile, args)

    {:noreply, room}
  end

  def handle_cast({:auto_move, mobile, unities}, %Room{} = room) do
    case room.exits do
      nil ->
        Mobile.auto_move(mobile, [])
      exits ->
        valid_exits =
          case unities do
            [] ->
              Enum.filter(exits, fn(%{"direction" => direction}) ->
                room.room_unity.exits[direction] && room.room_unity.exits[direction]["area"] == room.area
              end)
            unities ->
              Enum.filter(exits, fn(%{"direction" => direction}) ->
                room.room_unity.exits[direction] && room.room_unity.exits[direction]["controlled_by"] in unities
              end)
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
              |> interpolate(%{
                   "name" => name,
                   "direction" => room |> get_direction_by_destination(from_room_id) |> enter_direction()
                 })
              |> capitalize_first

    ApathyDrive.PubSub.broadcast! "rooms:#{room.id}:mobiles", {:mobile_movement, %{mobile: mobile, room: room.id, message: "<p><span class='grey'>#{message}</span></p>"}}
    {:noreply, room}
  end

  def handle_cast({:mobile_left, %{name: name, mobile: mobile, message: message, to: to_room_id}}, room) do
    message = message
              |> interpolate(%{
                   "name" => name,
                   "direction" => room |> get_direction_by_destination(to_room_id) |> exit_direction()
                 })
              |> capitalize_first

    ApathyDrive.PubSub.broadcast! "rooms:#{room.id}:mobiles", {:mobile_movement, %{mobile: mobile, room: room.id, message: "<p><span class='grey'>#{message}</span></p>"}}
    {:noreply, room}
  end

  def handle_cast({:add_item, item}, %Room{items: items} = room) do
    room =
      put_in(room.items, [item | items])
      |> save!

    {:noreply, room}
  end

  def handle_cast({:add_items, new_items}, %Room{items: items} = room) do
    room =
      put_in(room.items, new_items ++ items)
      |> save!

    {:noreply, room}
  end

  def handle_info(:save, room) do
    Process.send_after(self, :save, jitter(:timer.minutes(30)))
    {:noreply, save!(room), :hibernate}
  end

  def handle_info(:spawn_monsters,
                  %{:lair_next_spawn_at => lair_next_spawn_at} = room) do

    if Date.to_secs(Date.now) >= lair_next_spawn_at do

      room_pid = self()

      Task.start_link fn ->
        ApathyDrive.LairSpawning.spawn_lair(room, room_pid)
      end

      room = room
             |> Map.put(:lair_next_spawn_at, Date.now
                                             |> Date.shift(mins: room.lair_frequency)
                                             |> Date.to_secs)
    end

    :erlang.send_after(5000, self, :spawn_monsters)

    {:noreply, room}
  end

  def handle_info({:door_bashed_open, %{direction: direction}}, room) do
    room = open!(room, direction)

    room_exit = get_exit(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_bash, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_bash, room_exit}, room) do
    room = open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_bash_failed, %{direction: direction}}, room) do
    room_exit = get_exit(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_bash_failed, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:door_opened, %{direction: direction}}, room) do
    room = open!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_open, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_open, room_exit}, room) do
    room = open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_closed, %{direction: direction}}, room) do
    room = close!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_close, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_close, room_exit}, room) do
    room = close!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_locked, %{direction: direction}}, room) do
    room = lock!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      ApathyDrive.PubSub.broadcast!("rooms:#{mirror_room.id}", {:mirror_lock, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_lock, room_exit}, room) do
    room = lock!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info(:execute_room_ability, %Room{room_ability: nil} = room) do
    ApathyDrive.PubSub.unsubscribe("rooms:abilities")

    {:noreply, room}
  end

  def handle_info(:spread_essence, %Room{exits: exits, room_unity: %RoomUnity{essences: essences}} = room) do
    essences =
      exits
      |> Enum.reduce(essences, fn
           %{"kind" => kind}, updated_essences when kind in ["Cast", "RemoteAction"] ->
             updated_essences
           %{"destination" => dest, "direction" => direction, "kind" => kind}, updated_essences ->
             essence_to_distribute =
               Enum.reduce(updated_essences, %{}, fn({unity, essence}, essence_to_distribute) ->
                 Map.put(essence_to_distribute, unity, div(essence, 100))
               end)

             updated_essence =
               Enum.reduce(updated_essences, %{}, fn({unity, essence}, updated_essence) ->
                 Map.put(updated_essence, unity, essence - essence_to_distribute[unity])
               end)

             if (default = default_essence(room)) > 0 do
               updated_essence = Map.put(updated_essence, "default", default)
             end

             payload = %{
               essence: essence_to_distribute,
               room_id: room.id,
               direction: direction,
               kind: kind,
               legacy_id: room.legacy_id,
               area: room.area,
               controlled_by: room.room_unity.controlled_by
             }

             dest
             |> Room.find
             |> send({:spread_essence, payload})

             updated_essence
         end)

    room =
      put_in(room.room_unity.essences, essences)
      |> essence_reaction()
      |> update_controlled_by()

    {:noreply, room}
  end

  def handle_info({:spread_essence, distribution}, %Room{room_unity: %RoomUnity{essences: essences}} = room) do
    updated_essences =
      Enum.reduce(distribution.essence, essences, fn({unity, essence}, updated_essences) ->
        Map.put(updated_essences, unity, updated_essences[unity] + essence)
      end)

    if (default = default_essence(room)) > 0 do
      updated_essences = Map.put(updated_essences, "default", default)
    end

    mirror_exit =
      mirror_exit(room, distribution.room_id)

    unless mirror_exit do
      expected_direction = ApathyDrive.Exit.reverse_direction(distribution.direction)

      if room.exits |> Enum.map(&(&1["direction"])) |> Enum.member?(expected_direction) do
        Logger.info "CONFLICTING DIRECTION"
      else
        new_exit = %{"kind" => distribution.kind, "direction" => expected_direction, "destination" => distribution.room_id}
        Logger.info "auto-creating exit: #{inspect new_exit}"

        if distribution.kind == "Normal" do
          put_in(room.exits, [new_exit | room.exits])
          |> Repo.save!
        end
      end

      raise "expected exit #{expected_direction} from #{room.id} (#{room.legacy_id}) to #{distribution.room_id} (#{distribution.legacy_id})"
    end

    room = put_in(room.room_unity.essences, updated_essences)
    room = put_in(room.room_unity.exits[mirror_exit["direction"]], %{"area" => distribution.area, "controlled_by" => distribution.controlled_by})
           |> update_controlled_by()

    {:noreply, room}
  end

  def handle_info(:execute_room_ability, %Room{room_ability: ability} = room) do
    ApathyDrive.PubSub.broadcast!("rooms:#{room.id}:spirits", {:execute_room_ability, ability})

    {:noreply, room}
  end

  def handle_info({:timeout, _ref, {name, time, [module, function, args]}}, %Room{timers: timers} = room) do
    jitter = trunc(time / 2) + :random.uniform(time)

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

end
