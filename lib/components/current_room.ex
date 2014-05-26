defmodule Components.CurrentRoom do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.CurrentRoom, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_current_room, new_value})
  end

  def get_current_room(entity) do
    :gen_event.call(entity, Components.CurrentRoom, :get_current_room)
  end

  def set_current_room(entity, room_pid) do
    value(entity, room_pid |> Components.ID.value)
    Entity.save!(entity)
  end

  def serialize(entity) do
    %{"CurrentRoom" => value(entity)}
  end

  ### GenEvent API
  def init(room_pid) do
    {:ok, room_pid}
  end

  def handle_call(:value, room_id) do
    {:ok, room_id, room_id}
  end

  def handle_call(:get_current_room, room_id) do
    {:ok, Components.find_by(Components.ID, room_id), room_id}
  end

  def handle_event({:set_current_room, new_room}, _room) do
    {:ok, new_room}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
