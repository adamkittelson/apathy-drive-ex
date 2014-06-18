defmodule Races do
  use Systems.Reload
  use GenServer

  # Public API
  def add(race) do
    GenServer.cast(:races, {:add, race})
  end

  def all do
    GenServer.call(:races, :all)
  end

  def find_by_name(name) do
    Enum.find(all, fn (race) ->
      Components.Name.value(race) == name
    end)
  end

  def find_by_module(module) do
    Enum.find(all, fn (race) ->
      Components.Module.value(race) == module
    end)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Races, [], name: :races)
  end

  def init(races) do
    {:ok, races}
  end

  def handle_cast({:add, race}, races) do
    {:noreply, [race | races] }
  end

  def handle_call(:all, _from, races) do
    {:reply, races, races}
  end

end
