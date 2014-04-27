defmodule Components.Description do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Description, :get_description)
  end

  def get_description(entity) do
    value(entity)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_description, new_value})
  end

  def serialize(entity) do
    {"Description", get_description(entity)}
  end

  ### GenEvent API
  def init(description) do
    {:ok, description}
  end

  def handle_call(:get_description, description) do
    {:ok, description, description}
  end

  def handle_event({:set_description, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
