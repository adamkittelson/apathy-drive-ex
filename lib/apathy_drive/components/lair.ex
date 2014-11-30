defmodule Components.Lair do
  use Systems.Reload
  use GenEvent
  use Timex

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Lair, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_lair, new_value})
  end

  def size(entity) do
    value(entity)["size"]
  end

  def frequency(entity) do
    value(entity)["frequency"]
  end

  def monster_ids(entity) do
    value(entity)["monsters"]
  end

  def last_spawned_at(entity) do
    value(entity)["last_spawned_at"]
  end

  def set_last_spawned_at(entity) do
    GenEvent.notify(entity, :set_last_spawned_at)
  end

  def monster_templates(entity) do
    monster_ids(entity) |> Enum.map(&(MonsterTemplates.find_by_id(&1)))
  end

  def monster_names(entity) do
    entity |> monster_templates |> Enum.map(&(Components.Name.value(&1)))
  end

  def set_lair_monsters(entity, lair_monsters) do
    value(entity, Enum.map(lair_monsters, &(&1 |> Components.ID.value)))
  end

  def serialize(entity) do
    value = value(entity)
    if value["last_spawned_at"] do
      %{"Lair" => put_in(value(entity)["last_spawned_at"], Date.convert(value(entity)["last_spawned_at"], :secs))}
    else
      %{"Lair" => value}
    end
  end

  ### GenEvent API
  def init(value) do
    if value["last_spawned_at"] do
      {:ok, put_in(value["last_spawned_at"], Date.from(value["last_spawned_at"], :secs))}
    else
      {:ok, value}
    end
  end

  def handle_call(:value, lair) do
    {:ok, lair, lair}
  end

  def handle_event({:set_lair, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(:set_last_spawned_at, value) do
    {:ok, Map.put(value, "last_spawned_at", Date.now) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
