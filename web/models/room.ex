defmodule Room do
  require Logger
  use Ecto.Model
  use Systems.Reload
  use GenServer
  use Timex
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "rooms" do
    field :name,                  :string
    field :keywords,              {:array, :string}
    field :description,           :string
    field :effects,               :any, virtual: true, default: %{}
    field :light,                 :integer
    field :item_descriptions,     ApathyDrive.JSONB
    field :placed_items,          {:array, :string}
    field :lair_size,             :integer
    field :lair_monsters,         {:array, :integer}
    field :lair_frequency,        :integer
    field :lair_next_spawn_at,    :any, virtual: true, default: 0
    field :permanent_npc,         :integer
    field :room_ability,          :string
    field :start_room,            :boolean, default: false
    field :shop_items,            {:array, :integer}
    field :trainable_skills,      {:array, :string}
    field :exits,                 ApathyDrive.JSONB
    field :legacy_id,             :string
    field :timers,                :any, virtual: true, default: %{}

    timestamps

    has_many :monsters, Monster
  end

  def init(%Room{} = room) do
    PubSub.subscribe(self, "rooms")
    PubSub.subscribe(self, "rooms:#{room.id}")
    send(self, :load_monsters)

    if room.lair_monsters do
      PubSub.subscribe(self, "rooms:lairs")
      send(self, {:spawn_monsters, Date.now |> Date.convert(:secs)})
    end

    if room.permanent_npc do
      PubSub.subscribe(self, "rooms:permanent_npcs")
      send(self, :spawn_permanent_npc)
    end

    {:ok, room}
  end

  def start_room_id do
    query = from r in Room,
            where: r.start_room == true,
            select: r.id

    Repo.one(query)
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
    case Repo.get(Room, id) do
      %Room{} = room ->

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"room_#{id}", {GenServer, :start_link, [Room, room, [name: {:global, :"room_#{id}"}]]}, :permanent, 5000, :worker, [Room]})

        pid
      nil ->
        nil
    end
  end

  def all do
    PubSub.subscribers("rooms")
  end

  def value(room) do
    GenServer.call(room, :value)
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
  def items(%Room{} = room),    do: PubSub.subscribers("rooms:#{room.id}:items")
  def monsters(%Room{} = room, monster \\ nil) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Enum.reject(&(&1 == monster))
  end

  def shop?(%Room{shop_items: nil}),          do: false
  def shop?(%Room{shop_items: _}),            do: true
  def trainer?(%Room{trainable_skills: nil}), do: false
  def trainer?(%Room{trainable_skills: _}),   do: true

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
        |> Enum.shuffle
        |> List.first
    end
  end

  def look(%Room{} = room, %Spirit{} = spirit) do
    html = ~s(<div class='room'><div class='title'>#{room.name}</div><div class='description'>#{room.description}</div>#{look_shop_hint(room)}#{look_items(room)}#{look_monsters(room)}#{look_directions(room)}</div>)

    Spirit.send_scroll spirit, html
  end

  def look(%Room{} = room, %Monster{} = monster) do
    html = ~s(<div class='room'><div class='title'>#{room.name}</div><div class='description'>#{room.description}</div>#{look_shop_hint(room)}#{look_items(room)}#{look_monsters(room, monster.pid)}#{look_directions(room)}</div>)

    Monster.send_scroll(monster, html)
  end

  def look_shop_hint(%Room{shop_items: nil, trainable_skills: nil}), do: nil
  def look_shop_hint(%Room{}) do
    "<p><br><em>Type 'list' to see a list of goods and services sold here.</em><br><br></p>"
  end

  def look_items(%Room{} = room) do
    items = items(room)
            |> Enum.map(&Systems.Item.item_name/1)

    case Enum.count(items) do
      0 ->
        ""
      _ ->
        "<div class='items'>You notice #{Enum.join(items, ", ")} here.</div>"
    end
  end

  def look_monsters(%Room{} = room, monster \\ nil) do
    monsters = monsters(room, monster)
               |> Enum.map(&Monster.value/1)
               |> Enum.map(&Monster.look_name/1)
               |> Enum.join("<span class='magenta'>, </span>")

    case(monsters) do
      "" ->
        ""
      monsters ->
        "<div class='monsters'><span class='dark-magenta'>Also here:</span> #{monsters}<span class='dark-magenta'>.</span></div>"
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

  defp open!(%Room{} = room, direction) do
    if open_duration = ApathyDrive.Exit.open_duration(room, direction) do
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

  defp close!(%Room{effects: effects} = room, direction) do
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

  defp unlock!(%Room{effects: effects} = room, direction) do
    unlock_duration = if open_duration = ApathyDrive.Exit.open_duration(room, direction) do
      open_duration
    else
      10#300
    end

    Systems.Effect.add(room, %{unlocked: direction}, unlock_duration)
    # todo: tell players in the room when it re-locks
    #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
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

  # GenServer callbacks
  def handle_info({:spawn_monsters, time},
                 %{:lair_next_spawn_at => lair_next_spawn_at} = room)
                 when time >= lair_next_spawn_at do

    ApathyDrive.LairSpawning.spawn_lair(room)

    room = room
           |> Map.put(:lair_next_spawn_at, Date.now
                                           |> Date.shift(mins: room.lair_frequency)
                                           |> Date.convert(:secs))

    {:noreply, room}
  end

  def handle_info(:spawn_permanent_npc, room) do

    mt = MonsterTemplate.find(room.permanent_npc)

    unless MonsterTemplate.limit_reached?(mt) do
      monster = MonsterTemplate.spawn_monster(mt, room)

      Monster.display_enter_message(room, monster)
    end

    {:noreply, room}
  end

  def handle_info(:load_monsters, room) do
    query = from m in assoc(room, :monsters), select: m.id

    query
    |> ApathyDrive.Repo.all
    |> Enum.each(fn(monster_id) ->
         Monster.find(monster_id)
       end)

    {:noreply, room}
  end

  def handle_info({:door_bashed_open, %{basher: monster, direction: direction}}, room) do
    room = open!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_bash, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_bash, room_exit}, room) do
    room = open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_bash_failed, %{basher: monster, direction: direction}}, room) do
    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_bash_failed, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:door_opened, %{opener: monster, direction: direction}}, room) do
    room = open!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_open, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_open, room_exit}, room) do
    room = open!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_closed, %{closer: monster, direction: direction}}, room) do
    room = close!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_close, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_close, room_exit}, room) do
    room = close!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_picked, %{picker: monster, direction: direction}}, room) do
    room = unlock!(room, direction)

    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_pick, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:mirror_pick, room_exit}, room) do
    room = unlock!(room, room_exit["direction"])
    {:noreply, room}
  end

  def handle_info({:door_pick_failed, %{picker: monster, direction: direction}}, room) do
    room_exit = ApathyDrive.Exit.get_exit_by_direction(room, direction)

    {mirror_room, mirror_exit} = ApathyDrive.Exit.mirror(room, room_exit)

    if mirror_exit["kind"] == room_exit["kind"] do
      Phoenix.PubSub.broadcast("rooms:#{mirror_room.id}", {:mirror_pick_failed, mirror_exit})
    end

    {:noreply, room}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Room{timers: timers} = room) do
    new_ref = :erlang.start_timer(time, self, {name, time, function})

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

  def handle_info(_message, room) do
    {:noreply, room}
  end

end
