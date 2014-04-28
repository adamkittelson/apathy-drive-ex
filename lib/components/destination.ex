defmodule Components.Destination do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Destination, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_destination, new_value})
  end

  def get_destination(entity) do
    :gen_event.call(entity, Components.Destination, :get_destination)
  end

  def set_destination(entity, room_pid) do
    value(entity, room_pid |> Components.ID.value)
  end

  def serialize(entity) do
    {"Destination", value(entity)}
  end

  ### GenEvent API
  def init(destination_id) do
    {:ok, destination_id}
  end

  def handle_call(:value, destination_id) do
    {:ok, destination_id, destination_id}
  end

  def handle_call(:get_destination, destination_id) do
    {:ok, Components.find_by(Components.ID, destination_id), destination_id}
  end

  def handle_event({:set_destination, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
