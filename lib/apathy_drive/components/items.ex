defmodule Components.Items do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Items, :value)
  end

  def get_items(entity) do
    entity
    |> value
    |> Enum.map(&get_item/1)
    |> Enum.filter(&(&1 != nil))
  end

  def get_item(item) when is_pid(item), do: item
  def get_item(item) when is_number(item) do
    Items.find_by_id(item)
  end

  def item_ids(entity) do
    value(entity)
    |> Enum.filter(&is_pid/1)
    |> Enum.filter(&(Entity.has_component?(&1, Components.ID)))
    |> Enum.map(&(Components.ID.value(&1)))
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_items, new_value})
  end

  def add_item(entity, item) do
    Parent.set(item, entity)
    GenEvent.notify(entity, {:add_item, item})
  end

  def remove_item(entity, item) do
    Parent.set(item, nil)
    GenEvent.notify(entity, {:remove_item, item})
  end

  def serialize(entity) do
    %{"Items" => item_ids(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, items) do
    {:ok, items, items}
  end

  def handle_event({:set_items, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_item, item}, value) do
    {:ok, [item | value] |> Enum.uniq }
  end

  def handle_event({:remove_item, item}, value) do
    {:ok, List.delete(value, item) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end