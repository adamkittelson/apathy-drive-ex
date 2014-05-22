defmodule ItemTemplates do
  use GenServer.Behaviour

  # Public API
  def add(item) do
    :gen_server.cast(:item_templates, {:add, item})
  end

  def remove(item) do
    :gen_server.cast(:item_templates, {:remove, item})
  end

  def all do
    :gen_server.call(:item_templates, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:item_templates, {:get, id})
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
    :gen_server.start_link({:local, :item_templates}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
  end

  def handle_cast({:add, item}, item_templates) do
    id = Components.ID.value(item)
    {:noreply, HashDict.put_new(item_templates, id, item) }
  end

  def handle_cast({:remove, item_template}, item_templates) do
    {:noreply, HashDict.delete(item_templates, Components.ID.value(item_template)) }
  end

  def handle_call(:all, _from, item_templates) do
    {:reply, HashDict.values(item_templates), item_templates}
  end

  def handle_call({:get, id}, _from, item_templates) do
    {:reply, HashDict.get(item_templates, id), item_templates}
  end

end