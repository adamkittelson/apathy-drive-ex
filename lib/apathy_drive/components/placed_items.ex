defmodule Components.PlacedItems do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.PlacedItems, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_placed_items, new_value})
  end

  def serialize(entity) do
    %{"PlacedItems" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, item_names) do
    {:ok, item_names, item_names}
  end

  def handle_event({:set_placed_items, new_item_names}, _value) do
    {:ok, new_item_names }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
