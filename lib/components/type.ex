defmodule Components.Type do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Type, :get_type)
  end

  def get_type(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_type, new_value})
  end

  def serialize(entity) do
    %{"Type" => get_type(entity)}
  end

  ### GenEvent API
  def init(type) do
    {:ok, type}
  end

  def handle_call(:get_type, type) do
    {:ok, type, type}
  end

  def handle_event({:set_type, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
