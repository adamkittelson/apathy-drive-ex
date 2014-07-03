defmodule Components do
  use Systems.Reload
  use GenServer

  # Public API
  def add(component, entity) do
    GenServer.cast(:components, {:add, component, entity, component.value(entity)})
  end

  def remove(component, entity) do
    GenServer.cast(:components, {:remove, component, entity, component.value(entity)})
  end

  def all do
    GenServer.call(:components, :all)
  end

  def all(component) do
    GenServer.call(:components, {:all, component})
  end

  def find_by(component, value) do
    find_all_by(component, value) |> first
  end

  def find_all_by(component, value) do
    GenServer.call(:components, {:find_by, component, value})
  end

  def find_by(components, component, value) do
    find_all_by(components, component, value) |> first
  end

  def first(components) do
    components |> HashSet.to_list |> List.first
  end

  def find_all_by(components, component, value) do
    components = Enum.into(components, HashSet.new)
    find_all_by(component, value) |> HashSet.intersection(components)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Components, HashDict.new, name: :components)
  end

  def init(components) do
    {:ok, components}
  end

  def handle_cast({:add, component, entity, value}, components) do
    current_hash = HashDict.get(components, component, HashDict.new)
    current_pids = HashDict.get(current_hash, value, HashSet.new)
    new_pids = Set.put(current_pids, entity)
    new_hash = HashDict.put(current_hash, value, new_pids)
    {:noreply, HashDict.put(components, component, new_hash) }
  end

  def handle_cast({:remove, component, entity, value}, components) do
    current_hash = HashDict.get(components, component, HashDict.new)
    current_pids = HashDict.get(current_hash, value, HashSet.new)
    new_pids = Set.delete(current_pids, entity)
    new_hash = HashDict.put(current_hash, value, new_pids)
    {:noreply, HashDict.put(components, component, new_hash) }
  end

  def handle_call(:all, _from, components) do
    {:reply, HashDict.keys(components), components}
  end

  def handle_call({:all, component}, _from, components) do
    list = HashDict.get(components, component)
    if list do
      list = list |> HashDict.values
                  |> List.flatten
                  |> Enum.map(&(HashSet.to_list(&1)))
                  |> List.flatten
                  |> Enum.into HashSet.new
    else
      list = HashSet.new
    end
    {:reply, list, components}
  end

  def handle_call({:find_by, component, value}, _from, components) do
    {:reply, HashDict.get(components, component, HashDict.new) |> HashDict.get(value, HashSet.new), components}
  end

end