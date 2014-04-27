defmodule Components do
  use GenServer.Behaviour

  # Public API
  def add(component, entity) do
    :gen_server.cast(:components, {:add, component, entity})
  end

  def all do
    :gen_server.call(:components, :all)
  end

  def all(component) do
    :gen_server.call(:components, {:all, component})
  end

  def find_by(component, value) do
    :gen_server.call(:components, {:find_by, component, value})
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :components}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
  end

  def handle_cast({:add, component, entity}, components) do
    current_pids = HashDict.get(components, component, HashDict.new)
    current_pids = HashDict.put_new(current_pids, component.value(entity), entity)
    {:noreply, HashDict.put(components, component, current_pids) }
  end

  def handle_call(:all, _from, components) do
    {:reply, HashDict.keys(components), components}
  end

  def handle_call({:all, component}, _from, components) do
    {:reply, HashDict.get(components, component) |> HashDict.values, components}
  end

  def handle_call({:find_by, component, value}, _from, components) do
    {:reply, HashDict.get(components, component) |> HashDict.get(value), components}
  end

end