defmodule Item do
  require Logger
  use Ecto.Model
  use GenServer
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub

  schema "items" do
    field :name,                  :string,  virtual: true
    field :description,           :string,  virtual: true
    field :keywords,              {:array, :string}, virtual: true
    field :equipped,              :boolean, default: false
    field :worn_on,               :string,  virtual: true
    field :required_skills,       :any,     virtual: true, default: nil
    field :cost,                  :integer, virtual: true
    field :pid,                   :any,     virtual: true
    field :effects,               :any,     virtual: true, default: %{}
    field :timers,                :any,     virtual: true, default: %{}
    field :light,                 :integer, virtual: true
    field :always_lit,            :boolean, virtual: true
    field :uses,                  :integer
    field :destruct_message,      :string,  virtual: true
    field :room_destruct_message, :string,  virtual: true
    field :can_pick_up,           :boolean, virtual: true

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
        ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items")
        ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
      else
        ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:inventory")
      end
    end

    PubSub.subscribe(self, "items")
    PubSub.subscribe(self, "item_template:#{item.item_template_id}")

    item = item
           |> Map.put(:pid, self)

    :global.register_name(:"item_#{item.id}", self)

    {:ok, item}
  end

  def value(item) do
    GenServer.call(item, :value)
  end

  def to_monster_inventory(item, %Monster{} = monster) do
    GenServer.call(item, {:to_monster_inventory, monster})
  end

  def to_room(item, %Room{} = room) do
    GenServer.call(item, {:to_room, room})
  end

  def insert(%Item{id: nil} = item) do
    ApathyDrive.Repo.insert(item)
  end
  def insert(%Item{} = item), do: item

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
    case Repo.one from i in Item, where: i.id == ^id do
      %Item{} = item ->

        it = item.item_template_id
             |> ItemTemplate.find
             |> ItemTemplate.value

        item = Map.merge(it, item, fn(_key, it_val, item_val) ->
                    item_val || it_val
                  end)
                  |> Map.from_struct
                  |> Enum.into(Keyword.new)

        item = struct(Item, item)

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"item_#{item.id}", {GenServer, :start_link, [Item, item, []]}, :transient, 5000, :worker, [Item]})

        if item.always_lit do
          Item.light(pid)
        end

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

  def light(item) do
    GenServer.call(item, :light)
  end

  def extinguish(item) do
    GenServer.call(item, :extinguish)
  end

  def lit?(%Item{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(fn(effect) ->
         Map.has_key?(effect, "light")
       end)
  end

  def handle_call(:value, _from, monster) do
    {:reply, monster, monster}
  end

  def handle_call(:light, _from, item) do
    cond do
      !item.light ->
        {:reply, :not_a_light, item}
      lit?(item) ->
        {:reply, :already_lit, item}
      !!item.uses ->
        TimerManager.call_every(item, {:light, 1000, fn ->
          send(self, :use)
        end})
        item = Systems.Effect.add(item, %{"light" => item.light, "timers" => [:light]})
        {:reply, item, item}
      true ->
        item = Systems.Effect.add(item, %{"light" => item.light})
        {:reply, item, item}
    end
  end

  def handle_call(:extinguish, _from, item) do
    cond do
      !item.light ->
        {:reply, :not_a_light, item}
      !lit?(item) ->
        {:reply, :not_lit, item}
      item.always_lit ->
        if !!item.destruct_message do
          monster = Monster.find(item.monster_id)

          send(monster, {:item_destroyed, item})
          send(self, :delete)

          {:reply, item, item}
        else
          {:reply, :always_lit, item}
        end
      true ->
        item = Systems.Effect.remove(item, :light)
        {:reply, item, item}
    end
  end

  def handle_call({:to_monster_inventory, %Monster{} = monster}, _from, item) do
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:inventory")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
    ApathyDrive.PubSub.unsubscribe(self, "rooms:#{item.room_id}:items")

    ApathyDrive.PubSub.subscribe(self, "monsters:#{monster.id}:inventory")

    item = item
           |> Map.put(:monster_id, monster.id)
           |> Map.put(:room_id, nil)
           |> save

    {:reply, self, item}
  end

  def handle_call({:to_room, %Room{} = room}, _from, item) do
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:inventory")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
    ApathyDrive.PubSub.unsubscribe(self, "rooms:#{item.room_id}:items")

    ApathyDrive.PubSub.subscribe(self, "rooms:#{room.id}:items")

    item = item
           |> Map.put(:monster_id, nil)
           |> Map.put(:room_id, room.id)
           |> save

    {:reply, self, item}
  end

  def handle_info(:use, %Item{uses: 0, monster_id: nil} = item) do
    send(self, :delete)

    {:noreply, item}
  end

  def handle_info(:use, %Item{uses: 0, room_id: nil} = item) do
    monster = Monster.find(item.monster_id)

    send(monster, {:item_destroyed, item})
    send(self, :delete)

    {:noreply, item}
  end

  def handle_info(:use, %Item{uses: uses} = item) do
    item = item
           |> Map.put(:uses, uses - 1)
           |> save

    {:noreply, item}
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
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Weapon Hand", :unequip)
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
      "Two Handed" ->
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Weapon Hand", :unequip)
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Off-Hand", :unequip)
      "Off-Hand" ->
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Two Handed", :unequip)
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:Off-Hand", :unequip)
      _ ->
        ApathyDrive.PubSub.broadcast!("monsters:#{item.monster_id}:equipped_items:#{item.worn_on}", :unequip)
    end
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:inventory")
    ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items")
    ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
    send(Monster.find(item.monster_id), {:item_equipped, item})
    item = item
           |> Map.put(:equipped, true)
           |> save

    {:noreply, item}
  end

  def handle_info(:unequip, item) do
    ApathyDrive.PubSub.subscribe(self, "monsters:#{item.monster_id}:inventory")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items")
    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{item.monster_id}:equipped_items:#{item.worn_on}")
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
