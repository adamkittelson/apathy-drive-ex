defmodule Monsters do
  use Systems.Reload
  use GenServer

  # Public API
  def add(monster) do
    id = Components.ID.value(monster)
    GenServer.cast(:monsters, {:add, id, monster})
  end

  def remove(monster) do
    id = Components.ID.value(monster)
    GenServer.cast(:monsters, {:remove, id})
  end

  def all do
    GenServer.call(:monsters, :all)
  end

  def find_by_id(id) do
    GenServer.call(:monsters, {:get, id})
  end

  def find_all_by_name(name) do
    Enum.filter(all, fn (monster) ->
      monster |> Components.Name.get_name
           |> String.downcase
           |> String.contains?(String.downcase(name))
    end)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Monsters, HashDict.new, name: :monsters)
  end

  def init(monsters) do
    {:ok, monsters}
  end

  def handle_cast({:add, id, monster}, monsters) do
    {:noreply, HashDict.put_new(monsters, id, monster) }
  end

  def handle_cast({:remove, id}, monsters) do
    {:noreply, HashDict.delete(monsters, id) }
  end

  def handle_call(:all, _from, monsters) do
    {:reply, HashDict.values(monsters), monsters}
  end

  def handle_call({:get, id}, _from, monsters) do
    {:reply, HashDict.get(monsters, id), monsters}
  end

end
