defmodule Monsters do
  use Systems.Reload
  use GenServer
  import BlockTimer

  # Public API
  def add(monster) do
    name = Components.Name.value(monster)
    if Entity.has_component?(monster, Components.ID) do
      id = Components.ID.value(monster)
      GenServer.cast(:monsters, {:add, %{"id" => id, "name" => name, "monster" => monster}})
    else
      GenServer.cast(:monsters, {:add, %{"name" => name, "monster" => monster}})
    end
    initialize_ai(monster)
    Systems.Regen.initialize_regen(monster)
  end

  def remove(monster) do
    name = Components.Name.value(monster)
    if Entity.has_component?(monster, Components.ID) do
      id = Components.ID.value(monster)
      GenServer.cast(:monsters, {:remove, %{"id" => id, "name" => name, "monster" => monster}})
    else
      GenServer.cast(:monsters, {:remove, %{"name" => name, "monster" => monster}})
    end
  end

  def initialize_ai(monster) do
    {:ok, brain} = Task.start fn ->
      Components.Brain.think(monster)
    end

    Entity.add_component(monster, Components.Brain, brain)
  end

  def all do
    GenServer.call(:monsters, :all)
  end

  def find_by_id(id) do
    GenServer.call(:monsters, {:get, id})
  end

  def count(name) when is_binary(name) do
    name
    |> find_by_id
    |> count
  end

  def count(nil), do: 0
  def count(monsters) when is_list(monsters) do
    length(monsters)
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

  def handle_cast({:add, %{"id" => id, "name" => name, "monster" => monster}}, monsters) do
    monsters = monsters
               |> HashDict.put_new(id, monster)
               |> update_in([name], &([monster | &1 || []]))
    {:noreply,  monsters}
  end

  def handle_cast({:add, %{"name" => name, "monster" => monster}}, monsters) do
    monsters = monsters
               |> update_in([name], &([monster | &1 || []]))
    {:noreply,  monsters}
  end

  def handle_cast({:remove, %{"id" => id, "name" => name, "monster" => monster}}, monsters) do
    monsters = monsters
               |> HashDict.delete(id)
               |> update_in([name], fn(list) ->
                                      list = list || []
                                      Enum.reject(list, fn(i) ->
                                        i == monster
                                      end)
                                    end)
    {:noreply,  monsters}
  end

  def handle_cast({:remove, %{"name" => name, "monster" => monster}}, monsters) do
    monsters = monsters
               |> update_in([name], fn(list) ->
                                      list = list || []
                                      Enum.reject(list, fn(i) ->
                                         i == monster
                                      end)
                                    end)
    {:noreply,  monsters}
  end

  def handle_call(:all, _from, monsters) do
    all = monsters
          |> HashDict.values
          |> List.flatten
          |> Enum.uniq

    {:reply, all, monsters}
  end

  def handle_call({:get, id}, _from, monsters) do
    {:reply, HashDict.get(monsters, id), monsters}
  end

end
