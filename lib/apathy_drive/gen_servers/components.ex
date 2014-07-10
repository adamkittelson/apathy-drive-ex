defmodule Components do
  use Systems.Reload
  use GenServer

  # Public API
  def add(component, entity) do
    GenServer.cast(:components, {:add, component, entity})
  end

  def remove(component, entity) do
    GenServer.cast(:components, {:remove, component, entity})
  end

  def all do
    GenServer.call(:components, :all)
  end

  def all(component) do
    GenServer.call(:components, {:all, component})
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Components, HashDict.new, name: :components)
  end

  def init(components) do
    {:ok, components}
  end

  def handle_cast({:add, component, entity}, components) do
    current_pids = HashDict.get(components, component, HashSet.new)
    new_pids = Set.put(current_pids, entity)
    {:noreply, HashDict.put(components, component, new_pids) }
  end

  def handle_cast({:remove, component, entity}, components) do
    current_pids = HashDict.get(components, component, HashSet.new)
    new_pids = Set.delete(current_pids, entity)
    {:noreply, HashDict.put(components, component, new_pids) }
  end

  def handle_call(:all, _from, components) do
    {:reply, HashDict.keys(components), components}
  end

  def handle_call({:all, component}, _from, components) do
    {:reply, HashDict.get(components, component, HashSet.new), components}
  end

end