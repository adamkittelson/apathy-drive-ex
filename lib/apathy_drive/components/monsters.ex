defmodule Components.Monsters do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Monsters, :value)
  end

  def get_monsters(entity) do
    value(entity)
    |> Enum.map(&Monsters.find_by_id(&1))
    |> Enum.filter(&(&1 != nil))
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_monsters, new_value})
  end

  def add_monster(entity, monster) do
    Parent.set(monster, entity)
    GenEvent.notify(entity, {:add_monster, Components.ID.value(monster)})
  end

  def remove_monster(entity, monster) do
    Parent.set(monster, nil)
    GenEvent.notify(entity, {:remove_monster, Components.ID.value(monster)})
  end

  def serialize(entity) do
    %{"Monsters" => value(entity)}
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