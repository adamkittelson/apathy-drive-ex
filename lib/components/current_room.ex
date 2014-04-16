defmodule Components.CurrentRoom do
  use GenEvent.Behaviour

  ### Public API
  def get_current_room(entity) do
    :gen_event.call(entity, Components.CurrentRoom, :get_current_room)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_current_room, new_value})
  end

  def serialize(entity) do
    room_pid = get_current_room(entity)
    {"CurrentRoom", Components.ID.value(room_pid)}
  end

  ### GenEvent API
  def init(room_pid) do
    {:ok, room_pid}
  end

  def handle_call(:get_current_room, room_pid) do
    room_pid = if is_integer(room_pid) do
      Rooms.find_by_id(room_pid)
    else
      room_pid
    end
    {:ok, room_pid, room_pid}
  end

  def handle_event({:set_current_room, new_room}, _room) do
    {:ok, new_room}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
