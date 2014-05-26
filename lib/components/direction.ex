defmodule Components.Direction do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Direction, :get_direction)
  end
  
  def get_direction(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_direction, new_value})
  end

  def serialize(entity) do
    %{"Direction" => get_direction(entity)}
  end

  ### GenEvent API
  def init(direction) do
    {:ok, direction}
  end

  def handle_call(:get_direction, direction) do
    {:ok, direction, direction}
  end

  def handle_event({:set_direction, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
