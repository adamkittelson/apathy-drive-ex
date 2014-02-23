defmodule Components.Destination do
  use GenEvent.Behaviour

  ### Public API
  def get_destination(entity) do
    :gen_event.call(entity, Components.Destination, :get_destination)
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
end
