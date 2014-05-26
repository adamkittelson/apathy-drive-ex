defmodule Components.Types do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Types, :get_types)
  end

  def get_types(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_types, new_value})
  end

  def serialize(entity) do
    %{"Types" => value(entity)}
  end

  ### GenEvent API
  def init(type) do
    {:ok, type}
  end

  def handle_call(:get_types, type) do
    {:ok, type, type}
  end

  def handle_event({:set_types, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
