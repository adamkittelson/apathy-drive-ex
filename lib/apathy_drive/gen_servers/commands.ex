defmodule Commands do
  use Systems.Reload
  use GenServer

  # Public API
  def add(command) do
    GenServer.cast(:commands, {:add, command})
  end

  def all do
    GenServer.call(:commands, :all)
  end

  # GenServer API
  def start_link do
    GenServer.start_link(Commands, [], name: :commands)
  end

  def init(commands) do
    {:ok, commands}
  end

  def handle_cast({:add, command}, commands) do
    {:noreply, [command | commands] }
  end

  def handle_call(:all, _from, commands) do
    {:reply, commands, commands}
  end

end
