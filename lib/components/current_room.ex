defmodule Components.CurrentRoom do
  use GenEvent.Behaviour

  ### Public API
  def get_current_room(entity) do
    :gen_event.call(entity, Components.CurrentRoom, :get_current_room)
  end

  ### GenEvent API
  def init(room_pid) do
    {:ok, room_pid}
  end

  def handle_call(:get_current_room, room_pid) do
    {:ok, room_pid, room_pid}
  end

  def handle_event({:set_current_room, new_room}, room) do
    {:ok, new_room}
  end

end
