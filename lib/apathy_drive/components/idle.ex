defmodule Components.Idle do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Idle, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_idle, new_value})
  end

  def add(entity, amount) do
    GenEvent.notify(entity, {:add_idle, amount})
  end

  def serialize(entity) do
    %{"Idle" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_idle, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_idle, amount}, value) do
    {:ok, value + amount }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
