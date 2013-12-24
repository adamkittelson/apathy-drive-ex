defmodule Components.Connection do
  use GenEvent.Behaviour

  ### Public API
  def get_connection(entity) do
    :gen_event.call(entity, Components.Connection, :get_connection)
  end

  ### GenEvent API
  def init(connection_pid) do
    {:ok, connection_pid}
  end

  def handle_call(:get_connection, connection_pid) do
    {:ok, connection_pid, connection_pid}
  end
end
