defmodule Items do
  use Systems.Reload
  use GenServer

  # Public API
  def add(item) do
    id = Components.ID.value(item)
    GenServer.cast(:items, {:add, id, item})
  end

  def remove(item) do
    id = Components.ID.value(item)
    GenServer.cast(:items, {:remove, id})
  end

  def all do
    GenServer.call(:items, :all)
  end

  def find_by_id(id) do
    GenServer.call(:items, {:get, id})
  end

  def find_all_by_name(name) do
    Enum.filter(all, fn (item) ->
      item |> Components.Name.get_name
           |> String.downcase
           |> String.contains?(String.downcase(name))
    end)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Items, HashDict.new, name: :items)
  end

  def init(items) do
    {:ok, items}
  end

  def handle_cast({:add, id, item}, items) do
    {:noreply, HashDict.put_new(items, id, item) }
  end

  def handle_cast({:remove, id}, items) do
    {:noreply, HashDict.delete(items, id) }
  end

  def handle_call(:all, _from, items) do
    {:reply, HashDict.values(items), items}
  end

  def handle_call({:get, id}, _from, items) do
    {:reply, HashDict.get(items, id), items}
  end

end
