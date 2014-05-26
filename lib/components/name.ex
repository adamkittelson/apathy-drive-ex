defmodule Components.Name do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Name, :get_name)
  end

  def get_name(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_name, new_value})
  end

  def serialize(entity) do
    %{"Name" => get_name(entity)}
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:get_name, name) do
    {:ok, name, name}
  end

  def handle_event({:set_name, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
