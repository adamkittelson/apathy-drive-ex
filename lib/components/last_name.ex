defmodule Components.LastName do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.LastName, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_last_name, new_value})
  end

  def serialize(entity) do
    %{"LastName" => value(entity)}
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:value, name) do
    {:ok, name, name}
  end

  def handle_event({:set_last_name, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
