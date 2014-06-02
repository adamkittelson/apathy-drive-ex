defmodule Components.Items do
  use Systems.Reload
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Items, :value)
  end

  def get_items(entity) do
    value(entity) |> Enum.map(&(Components.find_by(Components.ID, &1)))
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_items, new_value})
  end

  def add_item(entity, item) do
    Entity.notify(entity, {:add_item, Components.ID.value(item)})
  end

  def remove_item(entity, item) do
    Entity.notify(entity, {:remove_item, Components.ID.value(item)})
  end

  def serialize(entity) do
    %{"Items" => value(entity)}
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