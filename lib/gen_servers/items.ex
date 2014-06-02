defmodule Items do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(item) do
    id = Components.ID.value(item)
    :gen_server.cast(:items, {:add, id, item})
  end

  def remove(item) do
    id = Components.ID.value(item)
    :gen_server.cast(:items, {:remove, id})
  end

  def all do
    :gen_server.call(:items, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:items, {:get, id})
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
    :gen_server.start_link({:local, :items}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
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
