defmodule Components.LairMonsters do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.LairMonsters, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_lair_monsters, new_value})
  end

  def get_lair_monsters(entity) do
    :gen_event.call(entity, Components.LairMonsters, :get_lair_monsters)
  end

  def set_lair_monsters(entity, lair_monsters) do
    value(entity, Enum.map(lair_monsters, &(&1 |> Components.ID.value)))
  end

  def serialize(entity) do
    {"LairMonsters", value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, lair_monster_ids) do
    {:ok, lair_monster_ids, lair_monster_ids}
  end

  def handle_call(:get_lair_monsters, lair_monster_ids) do
    {:ok, Enum.map(lair_monster_ids, &(Monsters.find_by_id(&1))), lair_monster_ids}
  end

  def handle_event({:set_lair_monsters, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
