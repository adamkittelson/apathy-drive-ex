defmodule Components.Help do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Help, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_help, new_value})
  end

  def serialize(entity) do
    %{"Help" => value(entity)}
  end

  ### GenEvent API
  def init(state) do
    {:ok, state}
  end

  def handle_call(:value, state) do
    {:ok, state, state}
  end

  def handle_event({:set_help, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
