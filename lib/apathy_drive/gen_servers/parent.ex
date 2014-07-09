defmodule Parent do
  use Systems.Reload
  use GenServer

  # Public API
  def set(entity, nil) do
    GenServer.cast(:parent, {:unset, entity})
  end

  def set(entity, parent) do
    GenServer.cast(:parent, {:set, entity, parent})
  end

  def of(entity) do
    GenServer.call(:parent, {:get, entity})
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Parent, HashDict.new, name: :parent)
  end

  def init(parents) do
    {:ok, parents}
  end

  def handle_cast({:set, entity, parent}, parents) do
    {:noreply, HashDict.put(parents, entity, parent) }
  end

  def handle_cast({:unset, entity}, parents) do
    {:noreply, HashDict.delete(parents, entity) }
  end

  def handle_call({:get, entity}, _from, parents) do
    {:reply, HashDict.get(parents, entity), parents}
  end

end
