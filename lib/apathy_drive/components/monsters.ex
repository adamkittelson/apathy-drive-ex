defmodule Components.Monsters do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Monsters, :value)
  end

  def get_monsters(entity) do
    entity
    |> value
    |> Enum.map(&get_monster/1)
    |> Enum.filter(&(&1 != nil))
  end

  def get_monster(monster) when is_pid(monster), do: monster
  def get_monster(monster) when is_number(monster) do
    Monsters.find_by_id(monster)
  end

  def monster_ids(entity) do
    value(entity)
    |> Enum.filter(&is_pid/1)
    |> Enum.filter(&(Entity.has_component?(&1, Components.ID)))
    |> Enum.map(&(Components.ID.value(&1)))
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_monsters, new_value})
  end

  def add_monster(entity, monster) do
    Parent.set(monster, entity)
    GenEvent.notify(entity, {:add_monster, monster})
  end

  def remove_monster(entity, monster) do
    Parent.set(monster, nil)
    GenEvent.notify(entity, {:remove_monster, monster})
  end

  def serialize(entity) do
    %{"Monsters" => monster_ids(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, monsters) do
    {:ok, monsters, monsters}
  end

  def handle_event({:set_monsters, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_monster, monster}, value) do
    {:ok, [monster | value] |> Enum.uniq }
  end

  def handle_event({:remove_monster, monster}, value) do
    {:ok, List.delete(value, monster) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end