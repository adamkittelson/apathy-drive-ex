defmodule Components.Hints do
  use Systems.Reload
  use GenEvent

  defstruct active: %{}, inactive: []

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Hints, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_hints, new_value})
  end

  def deactivate(entity, hint) do
    GenEvent.notify(entity, {:deactivate_hint, hint})
  end

  def serialize(entity) do
    %{"Hints" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    value = Jazz.encode!(value) |> Jazz.decode!(as: Components.Hints)
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_hints, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:deactivate_hint, hint}, hints) do
    if Map.has_key?(hints.active, hint) do
      hints = update_in hints.active, &(Map.delete(&1, hint))
      hints = update_in hints.inactive, &([hint | &1])
    end
    {:ok, hints }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
