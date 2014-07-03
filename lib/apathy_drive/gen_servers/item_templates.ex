defmodule ItemTemplates do
  use Systems.Reload
  use GenServer

  # Public API
  def add(id, item) do
    GenServer.cast(:item_templates, {:add, id, item})
  end

  def remove(item) do
    id = Components.ID.value(item)
    GenServer.cast(:item_templates, {:remove, id})
  end

  def all do
    GenServer.call(:item_templates, :all)
  end

  def find_by_id(id) do
    GenServer.call(:item_templates, {:get, id})
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
    GenServer.start_link(ItemTemplates, HashDict.new, name: :item_templates)
  end

  def init(item_templates) do
    {:ok, item_templates}
  end

  def handle_cast({:add, id, item}, item_templates) do
    {:noreply, HashDict.put_new(item_templates, id, item) }
  end

  def handle_cast({:remove, id}, item_templates) do
    {:noreply, HashDict.delete(item_templates, id) }
  end

  def handle_call(:all, _from, item_templates) do
    {:reply, HashDict.values(item_templates), item_templates}
  end

  def handle_call({:get, id}, _from, item_templates) do
    {:reply, HashDict.get(item_templates, id), item_templates}
  end

end