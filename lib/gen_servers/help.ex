defmodule Help do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(entity) do
    :gen_server.cast(:help, {:add, entity})
  end

  def all do
    :gen_server.call(:help, :all)
  end

  def find(query) do
    Systems.Match.all(all, :name_contains, query)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :help}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, entity}, help) do
    {:noreply, [entity | help] }
  end

  def handle_call(:all, _from, help) do
    {:reply, help, help}
  end

end