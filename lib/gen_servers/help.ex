defmodule Help do
  use Systems.Reload
  use GenServer

  # Public API
  def add(entity) do
    GenServer.cast(:help, {:add, entity})
  end

  def all do
    GenServer.call(:help, :all)
  end

  def find(query) do
    Systems.Match.all(all, :name_contains, query)
  end

  def find_by_module(module) do
    Enum.find(all, fn (help) ->
      Components.Module.value(help) == module
    end)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Help, [], name: :help)
  end

  def init(help) do
    {:ok, help}
  end

  def handle_cast({:add, entity}, help) do
    {:noreply, [entity | help] }
  end

  def handle_call(:all, _from, help) do
    {:reply, help, help}
  end

end