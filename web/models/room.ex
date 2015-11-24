defmodule Room do
  require Logger
  use ApathyDrive.Web, :model
  use GenServer
  use Timex
  alias ApathyDrive.PubSub
  alias ApathyDrive.Mobile
  alias ApathyDrive.ItemDrop

  schema "rooms" do
    field :name,                  :string
    field :keywords,              {:array, :string}
    field :description,           :string
    field :effects,               :any, virtual: true, default: %{}
    field :light,                 :integer
    field :item_descriptions,     ApathyDrive.JSONB, default: %{"hidden" => %{}, "visible" => %{}}
    field :lair_size,             :integer
    field :lair_monsters,         {:array, :integer}
    field :lair_frequency,        :integer, default: 5
    field :lair_next_spawn_at,    :any, virtual: true, default: 0
    field :lair_faction,          :string
    field :exits,                 ApathyDrive.JSONB, default: []
    field :commands,              ApathyDrive.JSONB, default: %{}
    field :legacy_id,             :string
    field :timers,                :any, virtual: true, default: %{}
    field :room_ability,          :any, virtual: true
    field :items,                 ApathyDrive.JSONB, default: []

    timestamps

    has_many   :monsters, Monster
    belongs_to :ability,  Ability
  end

  def init(id) do
    :random.seed(:os.timestamp)

    room = Repo.get!(Room, id)
    PubSub.subscribe(self, "rooms")
    PubSub.subscribe(self, "rooms:#{room.id}")

    if room.lair_monsters && Enum.any?(room.lair_monsters) do
      send(self, {:spawn_monsters, Date.now |> Date.to_secs})
    end

    room = if room.lair_monsters && Enum.any?(room.lair_monsters) do
      PubSub.subscribe(self, "rooms:lairs")
      TimerManager.call_every(room, {:spawn_monsters, 60_000, fn -> send(self, {:spawn_monsters, Date.now |> Date.to_secs}) end})
    else
      room
    end
    #
    # room = if room.ability_id do
    #   PubSub.subscribe(self, "rooms:abilities")
    #
    #   room
    #   |> Map.put(:room_ability, ApathyDrive.Repo.get(Ability, room.ability_id))
    #   |> TimerManager.call_every({:execute_room_ability, 5_000, fn -> send(self, :execute_room_ability) end})
    # else
    #   room
    # end

    {:ok, room}
  end

  def changeset(%Room{} = room, params \\ :empty) do
    room
    |> cast(params, ~w(name description exits), ~w(light item_descriptions lair_size lair_monsters lair_frequency lair_faction commands legacy_id))
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def start_room_id do
    ApathyDrive.Config.get(:start_room)
  end

  def find(id) do
    case :global.whereis_name(:"room_#{id}") do
      :undefined ->
        load(id)
      room ->
        room
    end
  end

  def load(id) do
    case Supervisor.start_child(ApathyDrive.Supervisor, {:"room_#{id}", {GenServer, :start_link, [Room, id, [name: {:global, :"room_#{id}"}]]}, :permanent, 5000, :worker, [Room]}) do
      {:error, {:already_started, pid}} ->
        pid
      {:ok, pid} ->
        # Hack to give the newly spawned pid a chance to handle messages in its mailbox before returning it
        # e.g. load monsters etc
        :timer.sleep(50)
        pid
    end
  end

  def all do
    PubSub.subscribers("rooms")
  end

  def value(room) do
    GenServer.call(room, :value)
  end

  def get_look_data(room, mobile) do
    GenServer.call(room, {:look_data, mobile})
  end

  def get_item(room, item) do
    GenServer.call(room, {:get_item, item})
  end

  def find_item(room, item) do
    GenServer.call(room, {:find_item, item})
  end

  def get_exit(room, direction) when is_pid(room) do
    GenServer.call(room, {:get_exit, direction})
  end
  def get_exit(%Room{} = room, direction) do
    Enum.find(room.exits, &(&1["direction"] == direction(direction)))
  end

  def mirror_exit(room, destination_id) do
    GenServer.call(room, {:get_mirror_exit, destination_id})
  end

  def command_exit(room, string) do
    GenServer.call(room, {:command_exit, string})
  end

  def add_item(room, item) do
    GenServer.cast(room, {:add_item, item})
  end

  def add_items(room, items) do
    GenServer.cast(room, {:add_items, items})
  end

  def unlocked?(room, direction) when is_pid(room) do
    GenServer.call(room, {:unlocked?, direction})
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

  def temporarily_open?(room, direction) when is_pid(room) do
    GenServer.call(room, {:temporarily_open?, direction})
  end
  def temporarily_open?(%Room{} = room, direction) do
    room.effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :open)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :open)
       end)
    |> Enum.member?(direction)
  end

  def html(room, mobile) do
    data = get_look_data(room, mobile)

    ~s(<div class='room'><div class='title'>#{data.lair_indicator}#{data.name}</div><div class='description'>#{data.description}</div>#{data.items}#{data.mobiles}#{data.exits}#{data.light}</div>)
  end

  def exit_direction("up"),      do: "upwards"
  def exit_direction("down"),    do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction(nil),       do: "nowhere"
  def enter_direction("up"),      do: "above"
  def enter_direction("down"),    do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def spawned_monsters(room_id) when is_integer(room_id), do: PubSub.subscribers("rooms:#{room_id}:spawned_monsters")
  def spawned_monsters(room),   do: PubSub.subscribers("rooms:#{id(room)}:spawned_monsters")

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

  def exit_directions(%Room{} = room) do
    room.exits
    |> Enum.map(fn(room_exit) ->
         :"Elixir.ApathyDrive.Exits.#{room_exit["kind"]}".display_direction(room, room_exit)
       end)
    |> Enum.reject(&(&1 == nil))
  end

  def random_direction(%Room{} = room) do
    :random.seed(:os.timestamp)

    case room.exits do
      nil ->
        nil
      exits ->
        exits
        |> Enum.map(&(&1["direction"]))
        |> Enum.random
    end
  end

  # def look(%Room{light: light} = room, %Spirit{} = spirit) do
  #   html = ~s(<div class='room'><div class='title'>#{lair_indicator(room)}#{room.name}</div><div class='description'>#{room.description}</div>#{look_items(room)}#{look_monsters(room, nil)}#{look_directions(room)}#{light_desc(light)}</div>)
  #
  #   Spirit.send_scroll spirit, html
  # end
  #
  # def look(%Room{light: light} = room, %Monster{} = monster) do
  #   html = if Monster.blind?(monster) do
  #     "<p>You are blind.</p>"
  #   else
  #     ~s(<div class='room'><div class='title'>#{lair_indicator(room)}#{room.name}</div><div class='description'>#{room.description}</div>#{look_items(room)}#{look_monsters(room, monster)}#{look_directions(room)}#{light_desc(light)}</div>)
  #   end
  #
  #   Monster.send_scroll(monster, html)
  # end

  def lair_indicator(%Room{lair_monsters: nil}), do: nil
  def lair_indicator(%Room{lair_faction: nil}) do
    "<span class='grey'>*</span>"
  end
  def lair_indicator(%Room{lair_faction: "Demon"}) do
    "<span class='magenta'>*</span>"
  end
  def lair_indicator(%Room{lair_faction: "Angel"}) do
    "<span class='white'>*</span>"
  end
  def lair_indicator(%Room{lair_faction: "Elemental"}) do
    "<span class='dark-cyan'>*</span>"
  end

  def light_desc(light_level)  when light_level <= -100, do: "<div>The room is barely visible</div>"
  def light_desc(light_level)  when light_level <=  -25, do: "<div>The room is dimly lit</div>"
  def light_desc(_light_level), do: nil

  def look_items(%Room{} = room) do
    psuedo_items = room.item_descriptions["visible"]
                   |> Map.keys

    items = case room.items do
      nil ->
        []
      items ->
        items
        |> Enum.map(&(&1["name"]))
    end

    items = items ++ psuedo_items

    case Enum.count(items) do
      0 ->
        ""
      _ ->
        "<div class='items'>You notice #{Enum.join(items, ", ")} here.</div>"
    end
  end

  def look_mobiles(%{room_id: room_id, mobile: pid}) do
    mobiles = mobiles(%{room_id: room_id, mobile: pid})
              |> Enum.map(&Mobile.look_name/1)
              |> Enum.join("<span class='magenta'>, </span>")

    case(mobiles) do
      "" ->
        ""
      mobiles ->
        "<div class='monsters'><span class='dark-magenta'>Also here:</span> #{mobiles}<span class='dark-magenta'>.</span></div>"
    end
  end

  def look_mobiles(%Room{} = room) do
    mobiles = mobiles(room)
               |> Enum.map(&Mobile.look_name/1)
               |> Enum.join("<span class='magenta'>, </span>")

    case(mobiles) do
      "" ->
        ""
      mobiles ->
        "<div class='monsters'><span class='dark-magenta'>Also here:</span> #{mobiles}<span class='dark-magenta'>.</span></div>"
    end
  end

  def look_directions(%Room{} = room) do
    case exit_directions(room) do
      [] ->
        "<div class='exits'>Obvious exits: NONE</div>"
      directions ->
        "<div class='exits'>Obvious exits: #{Enum.join(directions, ", ")}</div>"
    end
  end

  def send_scroll(room, html) do
    ApathyDrive.Endpoint.broadcast! "rooms:#{Room.id(room)}:mobiles", "scroll", %{:html => html}
  end

  def open!(room, direction) when is_pid(room) do
    GenServer.call(room, {:open, direction})
  end
  def open!(%Room{} = room, direction) do
    if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      Systems.Effect.add(room, %{open: direction}, open_duration)
      # todo: tell players in the room when it re-locks
      #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
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

  def close!(room, direction) when is_pid(room) do
    GenServer.call(room, {:close, direction})
  end
  def close!(%Room{effects: effects} = room, direction) do
    room = effects
           |> Map.keys
           |> Enum.filter(fn(key) ->
                effects[key][:open] == direction
              end)
           |> Enum.reduce(room, fn(room, key) ->
                Systems.Effect.remove(room, key)
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

  def lock!(room, direction) when is_pid(room) do
    GenServer.call(room, {:lock, direction})
  end
  def lock!(%Room{effects: effects} = room, direction) do
    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:unlocked] == direction
       end)
    |> Enum.reduce(room, fn(key, room) ->
         Systems.Effect.remove(room, key)
       end)
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

  def all_monsters_belong_to_faction?(%Room{id: id}, faction) do
    monster_count = ApathyDrive.PubSub.subscribers("rooms:#{id}:monsters") |> length
    case faction do
      "Demon" ->
        monster_count == ApathyDrive.PubSub.subscribers("rooms:#{id}:monsters:evil") |> length
      "Angel" ->
        monster_count == ApathyDrive.PubSub.subscribers("rooms:#{id}:monsters:good") |> length
      "Elemental" ->
        monster_count == ApathyDrive.PubSub.subscribers("rooms:#{id}:monsters:neutral") |> length
    end
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

  # Generate functions from Ecto schema
  fields = Keyword.keys(@struct_fields) -- Keyword.keys(@ecto_assocs)

  Enum.each(fields, fn(field) ->
    def unquote(field)(pid) do
      GenServer.call(pid, unquote(field))
    end

    def unquote(field)(pid, new_value) do
      GenServer.call(pid, {unquote(field), new_value})
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, state) do
      {:reply, Map.get(state, unquote(field)), state}
    end

    def handle_call({unquote(field), new_value}, _from, state) do
      {:reply, new_value, Map.put(state, unquote(field), new_value)}
    end
  end)

  def handle_call(:value, _from, room) do
    {:reply, room, room}
  end

  def handle_call({:get_item, item}, _from, %Room{items: items, item_descriptions: item_descriptions} = room) do
    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Systems.Match.one(:name_contains, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Systems.Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Systems.Match.one(:keyword_starts_with, item)

    cond do
      visible_item ->
        {:reply, {:cant_get, visible_item.name}, room}
      hidden_item ->
        {:reply, {:cant_get, hidden_item.name}, room}
      actual_item ->
        room =
          room
          |> Map.put(:items, List.delete(room.items, actual_item.item))
          |> Repo.update!
        {:reply, actual_item.item, room}
      true ->
        {:reply, :not_found, room}
    end
  end

  def handle_call({:find_item, item}, _from, %Room{items: items, item_descriptions: item_descriptions} = room) do
    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Systems.Match.one(:keyword_starts_with, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Systems.Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Systems.Match.one(:keyword_starts_with, item)

    item = cond do
      visible_item ->
        item_descriptions["visible"][visible_item.name]
      hidden_item ->
        item_descriptions["hidden"][hidden_item.name]
      actual_item ->
        actual_item.item
      true ->
        nil
    end
    {:reply, item, room}
  end

  def handle_call({:temporarily_open?, direction}, _from, room) do
    {:reply, temporarily_open?(room, direction), room}
  end

  def handle_call({:unlocked?, direction}, _from, room) do
    {:reply, unlocked?(room, direction), room}
  end

  def handle_call({:open, direction}, _from, room) do
    room = open!(room, direction)
    {:reply, room, room}
  end

  def handle_call({:close, direction}, _from, room) do
    room = close!(room, direction)
    {:reply, room, room}
  end

  def handle_call({:lock, direction}, _from, room) do
    room = lock!(room, direction)
    {:reply, room, room}
  end

  def handle_call({:look_data, mobile}, _from, room) do
    data = %{
      lair_indicator: lair_indicator(room),
      name: room.name,
      description: room.description,
      items: look_items(room),
      mobiles: look_mobiles(%{room_id: room.id, mobile: mobile}),
      exits: look_directions(room),
      light: light_desc(room.light)
    }

    {:reply, data, room}
  end

  def handle_call({:get_exit, direction}, _from, room) do
    room_exit =  get_exit(room, direction)

    {:reply, room_exit, room}
  end

  def handle_call({:get_mirror_exit, destination_id}, _from, room) do
    room_exit = room.exits
                |> Enum.find(fn(%{"destination" => destination, "kind" => kind}) ->
                     destination == destination_id and kind != "RemoteAction"
                   end)

    {:reply, room_exit, room}
  end

  def handle_call({:command_exit, string}, _from, room) do
    command_exit = room.exits
                   |> Enum.find(fn(ex) ->
                        ex["kind"] == "Command" and Enum.member?(ex["commands"], string)
                      end)

    {:reply, command_exit, room}
  end

  def handle_cast({:add_item, item}, %Room{items: items} = room) do
    room =
      put_in(room.items, [item | items])
      |> Repo.update!

    {:noreply, room}
  end

  def handle_cast({:add_items, new_items}, %Room{items: items} = room) do
    room =
      put_in(room.items, new_items ++ items)
      |> Repo.update!

    {:noreply, room}
  end

  # GenServer callbacks
  def handle_info({:spawn_monsters, time},
                 %{:lair_next_spawn_at => lair_next_spawn_at} = room)
                 when time >= lair_next_spawn_at do

    ApathyDrive.LairSpawning.spawn_lair(room)

    room = room
           |> Map.put(:lair_next_spawn_at, Date.now
                                           |> Date.shift(mins: room.lair_frequency)
                                           |> Date.to_secs)

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
    ApathyDrive.PubSub.unsubscribe(self, "rooms:abilities")

    {:noreply, room}
  end

  def handle_info(:execute_room_ability, %Room{room_ability: ability} = room) do
    ApathyDrive.PubSub.broadcast!("rooms:#{room.id}:monsters", {:execute_room_ability, ability})

    {:noreply, room}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Room{timers: timers} = room) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %Room{timers: timers} = room) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(room, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, room) do
    room = Systems.Effect.remove(room, key)
    {:noreply, room}
  end

  def handle_info({:search, direction}, room) do
    room = Systems.Effect.add(room, %{searched: direction}, 300)
    {:noreply, room}
  end

  def handle_info({:trigger, direction}, room) do
    room = Systems.Effect.add(room, %{triggered: direction}, 300)
    {:noreply, room}
  end

  def handle_info({:clear_triggers, direction}, room) do
    room = room.effects
           |> Map.keys
           |> Enum.filter(fn(key) ->
                room.effects[key][:triggered] == direction
              end)
           |> Enum.reduce(room, &(Systems.Effect.remove(&2, &1)))

    {:noreply, room}
  end

  def handle_info({:capture, monster: monster, faction: _}, %Room{lair_monsters: nil} = room) do
    send(monster, {:scroll, "<p>This room is not a lair.</p>"})
    {:noreply, room}
  end

  def handle_info({:capture, monster: monster, faction: faction}, %Room{lair_faction: lair_faction} = room) when faction == lair_faction do
    send(monster, {:scroll, "<p>The #{faction}s already control this lair.</p>"})

    {:noreply, room}
  end

  def handle_info({:capture, monster: monster, faction: faction}, %Room{lair_faction: _lair_faction} = room) do
    if all_monsters_belong_to_faction?(room, faction) do
      room =
        room
        |> Map.put(:lair_faction, faction)
        |> Repo.update!

      send(monster, {:scroll, "<p>You capture the lair for your faction!</p>"})
      ApathyDrive.Factions.update_war_status

      {:noreply, room}
    else
      send(monster, {:scroll, "<p>You must first clear the room of adversaries.</p>"})
      {:noreply, room}
    end
  end

  def handle_info({:room_updated, %{changes: changes}}, room) do
    {:noreply, Map.merge(room, changes)}
  end

  def handle_info({:generate_loot, monster_template_id, level}, room) do
    items =
      ItemDrop.monster_drops(monster_template_id)
      |> Enum.map(fn(%{item_id: item_id, chance: chance}) ->
           ApathyDrive.Item.generate_item(%{chance: chance, item_id: item_id, level: level})
         end)
      |> Enum.reject(&is_nil/1)

    room =
      room
      |> Map.put(:items, items ++ room.items)
      |> Repo.update!
    {:noreply, room}
  end

  def handle_info(_message, room) do
    {:noreply, room}
  end

end
