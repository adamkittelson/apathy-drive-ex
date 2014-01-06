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
    room_pid = if is_integer(room_pid) do
      :global.whereis_name(:"#{room_pid}")
    else
      room_pid
    end
    {:ok, room_pid, room_pid}
  end

  def handle_event({:set_current_room, new_room}, room) do
    {:ok, new_room}
  end

  def handle_event(_, room) do
    {:ok, room}
  end

end
