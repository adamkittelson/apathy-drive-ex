defmodule Abilities do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(ability) do
    :gen_server.cast(:abilities, {:add, ability})
  end

  def all do
    :gen_server.call(:abilities, :all)
  end

  # GenServer API
  def start_link do
    :gen_server.start_link({:local, :abilities}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, ability}, abilities) do
    {:noreply, [ability | abilities] }
  end

  def handle_call(:all, _from, abilities) do
    {:reply, abilities, abilities}
  end

end
