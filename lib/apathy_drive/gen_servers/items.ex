defmodule Items do
  use Systems.Reload
  use GenServer

  # Public API
  def add(item) do
    name = Components.Name.value(item)
    if Entity.has_component?(item, Components.ID) do
      id = Components.ID.value(item)
      GenServer.cast(:items, {:add, %{"id" => id, "name" => name, "item" => item}})
    else
      GenServer.cast(:items, {:add, %{"name" => name, "item" => item}})
    end
  end

  def remove(item) do
    name = Components.Name.value(item)
    if Entity.has_component?(item, Components.ID) do
      id = Components.ID.value(item)
      GenServer.cast(:items, {:remove, %{"id" => id, "name" => name, "item" => item}})
    else
      GenServer.cast(:items, {:remove, %{"name" => name, "item" => item}})
    end
  end

  def all do
    GenServer.call(:items, :all)
  end

  def find_by_id(id) do
    GenServer.call(:items, {:get, id})
  end

  def count(name) when is_binary(name) do
    name
    |> find_by_id
    |> count
  end

  def count(nil), do: 0
  def count(items) when is_list(items) do
    length(items)
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

  def handle_cast({:add, %{"id" => id, "name" => name, "item" => item}}, items) do
    items = items
            |> HashDict.put_new(id, item)
            |> update_in([name], &([item | &1 || []]))
    {:noreply,  items}
  end

  def handle_cast({:add, %{"name" => name, "item" => item}}, items) do
    items = items
            |> update_in([name], &([item | &1 || []]))
    {:noreply,  items}
  end

  def handle_cast({:remove, %{"id" => id, "name" => name, "item" => item}}, items) do
    items = items
            |> HashDict.delete(id)
            |> update_in([name], fn(list) ->
                                   list = list || []
                                   Enum.reject(list, fn(i) ->
                                     i == item
                                   end)
                                 end)
    {:noreply,  items}
  end

  def handle_cast({:remove, %{"name" => name, "item" => item}}, items) do
    items = items
            |> update_in([name], fn(list) ->
                                   list = list || []
                                   Enum.reject(list, fn(i) ->
                                      i == item
                                   end)
                                 end)
    {:noreply,  items}
  end

  def handle_call(:all, _from, items) do
    all = items
          |> HashDict.values
          |> List.flatten
          |> Enum.uniq

    {:reply, all, items}
  end

  def handle_call({:get, id}, _from, items) do
    {:reply, HashDict.get(items, id), items}
  end

end
