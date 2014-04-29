defmodule Components.Lair do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Lair, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_lair, new_value})
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
    ApathyDrive.Entity.notify(entity, :set_last_spawned_at)
  end

  def monster_templates(entity) do
    monster_ids(entity) |> Enum.map(&(Components.find_by(Components.ID, &1)))
  end

  def monster_names(entity) do
    entity |> monster_templates |> Enum.map(&(Components.Name.value(&1)))
  end

  def set_lair_monsters(entity, lair_monsters) do
    value(entity, Enum.map(lair_monsters, &(&1 |> Components.ID.value)))
  end

  def serialize(entity) do
    {"Lair", value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, lair) do
    {:ok, lair, lair}
  end

  def handle_event(:set_last_spawned_at, value) do
    {:ok, ListDict.put(value, "last_spawned_at", :os.timestamp) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
