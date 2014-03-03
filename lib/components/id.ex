defmodule Components.ID do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.ID, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_id, new_value})
  end

  def serialize(_entity) do
    nil
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_id, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
