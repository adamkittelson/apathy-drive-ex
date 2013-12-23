defmodule ApathyDrive.DestinationComponent do
  use GenEvent.Behaviour

  ### Public API
  def get_destination(entity) do
    :gen_event.call(entity, ApathyDrive.DestinationComponent, :get_destination)
  end

  ### GenEvent API
  def init(destination_id) do
    {:ok, destination_id}
  end

  def handle_call(:get_destination, destination) do
    if is_integer(destination) do
      destination = :global.whereis_name(:"#{destination}")
    end
    {:ok, destination, destination}
  end
end
