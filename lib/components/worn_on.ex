defmodule Components.WornOn do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.WornOn, :get_worn_on)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_worn_on, new_value})
  end

  def serialize(entity) do
    %{"WornOn" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:get_worn_on, value) do
    {:ok, value, value}
  end

  def handle_event({:set_value, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
