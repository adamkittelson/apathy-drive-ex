defmodule Components.Hints do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Hints, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_hints, new_value})
  end

  def add(entity, name, value) do
    GenEvent.notify(entity, {:add_hint, name, value})
  end

  def deactivate(entity, hint) do
    GenEvent.notify(entity, {:deactivate_hint, hint})
  end

  def serialize(entity) do
    %{"Hints" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_hints, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_hint, name, value}, hints) do
    unless Enum.member?(hints["inactive"], name) do
      hints = put_in hints, ["active", name], value
    end
    {:ok, hints }
  end

  def handle_event({:deactivate_hint, hint}, hints) do
    if Map.has_key?(hints["active"], hint) do
      hints = update_in hints, ["active"], &(Map.delete(&1, hint))
      hints = update_in hints, ["inactive"], &([hint | &1])
    end
    {:ok, hints }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
