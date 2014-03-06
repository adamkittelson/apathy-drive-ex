defmodule Components.Destination do
  use GenEvent.Behaviour

  ### Public API
  def get_destination(entity) do
    :gen_event.call(entity, Components.Destination, :get_destination)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_destination, new_value})
  end

  def serialize(entity) do
    {"Destination", get_destination(entity)}
  end

  ### GenEvent API
  def init(value) when is_number(value) do
    {:ok, :global.whereis_name(:"#{value}")}
  end

  def init(destination_id) do
    {:ok, destination_id}
  end

  def handle_call(:get_destination, destination) do
    {:ok, destination, destination}
  end

  def handle_event({:set_destination, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
