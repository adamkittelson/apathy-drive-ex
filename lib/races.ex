defmodule Races do
  use GenServer.Behaviour

  # Public API
  def add(race) do
    :gen_server.cast(:races, {:add, race})
  end

  def all do
    :gen_server.call(:races, :all)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :races}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, race}, races) do
    {:noreply, [race | races] }
  end

  def handle_call(:all, _from, races) do
    {:reply, races, races}
  end

end