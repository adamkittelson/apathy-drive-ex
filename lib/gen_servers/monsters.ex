defmodule Monsters do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(monster) do
    id = Components.ID.value(monster)
    :gen_server.cast(:monsters, {:add, id, monster})
  end

  def remove(monster) do
    id = Components.ID.value(monster)
    :gen_server.cast(:monsters, {:remove, id})
  end

  def all do
    :gen_server.call(:monsters, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:monsters, {:get, id})
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
    :gen_server.start_link({:local, :monsters}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
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
