defmodule Commands do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(command) do
    :gen_server.cast(:commands, {:add, command})
  end

  def all do
    :gen_server.call(:commands, :all)
  end

  # GenServer API
  def start_link do
    :gen_server.start_link({:local, :commands}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, command}, commands) do
    {:noreply, [command | commands] }
  end

  def handle_call(:all, _from, commands) do
    {:reply, commands, commands}
  end

end
