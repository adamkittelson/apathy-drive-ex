defmodule Item do
  require Logger
  use Ecto.Model
  use GenServer
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "items" do
    field :name,                :string,  virtual: true
    field :keywords,            {:array, :string}, virtual: true
    field :equipped,            :boolean, default: false
    field :worn_on,             :string,  virtual: true
    field :required_skills,     :any,     virtual: true, default: nil
    field :cost,                :integer, virtual: true
    field :pid,                 :any,     virtual: true
    field :effects,             :any,     virtual: true, default: %{}
    field :timers,              :any,     virtual: true, default: %{}

    timestamps

    belongs_to :room,    Room
    belongs_to :monster, Monster
    belongs_to :item_template, ItemTemplate
  end

  def init(%Item{} = item) do
    :random.seed(:os.timestamp)

    if item.room_id do
      PubSub.subscribe(self, "rooms:#{item.room_id}")
      PubSub.subscribe(self, "rooms:#{item.room_id}:items")
    end

    if item.monster_id do
      PubSub.subscribe(self, "monsters:#{item.monster_id}")
      PubSub.subscribe(self, "monsters:#{item.monster_id}:items")

      if item.equipped do
        Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items")
        Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
      else
        Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:inventory")
      end
    end

    PubSub.subscribe(self, "items")
    PubSub.subscribe(self, "item_template:#{item.item_template_id}")

    item = item
           |> Map.put(:pid, self)

    {:ok, item}
  end

  def value(item) do
    GenServer.call(item, :value)
  end

  def insert(item) do
    GenServer.call(item, :insert)
  end

  def save(item) when is_pid(item), do: item |> value |> save
  def save(%Item{id: id} = item) when is_integer(id) do
    Repo.update(item)
  end
  def save(%Item{} = item), do: item

  def delete(%Item{} = item), do: Repo.delete(item)

  def find(id) do
    case :global.whereis_name(:"item_#{id}") do
      :undefined ->
        load(id)
      item ->
        item
    end
  end

  def load(id) do
    case Repo.one from i in Item, where: i.id == ^id, preload: [:item_template] do
      %Item{} = item ->

        it = item.item_template_id
             |> ItemTemplate.find
             |> ItemTemplate.value

        item = Map.merge(it, item, fn(key, it_val, item_val) ->
                    item_val || it_val
                  end)
                  |> Map.from_struct
                  |> Enum.into(Keyword.new)

        item = struct(Item, item)

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"item_#{item.id}", {GenServer, :start_link, [Item, item, [name: {:global, :"item_#{id}"}]]}, :transient, 5000, :worker, [Item]})

        pid
      nil ->
        nil
    end
  end

  def find_room(%Item{room_id: room_id}) do
    room_id
    |> Room.find
    |> Room.value
  end

  def set_room_id(%Item{} = item, room_id) do
    PubSub.unsubscribe(self, "rooms:#{item.room_id}")
    PubSub.unsubscribe(self, "rooms:#{item.room_id}:items")
    PubSub.subscribe(self, "rooms:#{room_id}")
    PubSub.subscribe(self, "rooms:#{room_id}:items")
    Map.put(item, :room_id, room_id)
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

  def handle_call(:value, _from, monster) do
    {:reply, monster, monster}
  end

  def handle_call(:insert, _from, %Item{id: nil} = item) do
    item = ApathyDrive.Repo.insert(item)

    :global.register_name(:"item_#{item.id}", item.pid)

    {:reply, item, item}
  end
  def handle_call(:insert, _from, item) do
    {:reply, item, item}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Item{timers: timers} = item) do
    new_ref = :erlang.start_timer(time, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(item, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %Item{timers: timers} = item) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(item, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, item) do
    item = Systems.Effect.remove(item, key)
    {:noreply, item}
  end

  def handle_info({:apply_ability, %Ability{} = ability, %Monster{} = ability_user}, item) do
    item = Ability.apply_ability(item, ability, ability_user)

    {:noreply, item}
  end

  def handle_info(:equip, item) do
    case item.worn_on do
      "Weapon Hand" ->
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Weapon Hand", :unequip)
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
      "Two Handed" ->
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Weapon Hand", :unequip)
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Off-Hand", :unequip)
      "Off-Hand" ->
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:Off-Hand", :unequip)
      _ ->
        Phoenix.PubSub.broadcast("monsters:#{item.monster_id}:equipped_items:#{item.worn_on}", :unequip)
    end
    Phoenix.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:inventory")
    Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items")
    Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
    send(Monster.find(item.monster_id), {:item_equipped, item})
    item = item
           |> Map.put(:equipped, true)
           |> save

    {:noreply, item}
  end

  def handle_info(:unequip, item) do
    Phoenix.PubSub.subscribe(self, "monsters:#{item.monster_id}:inventory")
    Phoenix.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items")
    Phoenix.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
    send(Monster.find(item.monster_id), {:item_unequipped, item})

    item = item
           |> Map.put(:equipped, false)
           |> save

    {:noreply, item}
  end

  def handle_info(:delete, item) do
    delete(item)

    Process.exit(self, :normal)
    {:noreply, item}
  end

  def handle_info(_message, monster) do
    {:noreply, monster}
  end

end
