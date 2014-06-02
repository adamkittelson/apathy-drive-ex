defmodule MonsterTemplates do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(monster) do
    id = Components.ID.value(monster)
    :gen_server.cast(:monster_templates, {:add, id, monster})
  end

  def remove(monster_template) do
    id = Components.ID.value(monster_template)
    :gen_server.cast(:monster_templates, {:remove, id})
  end

  def all do
    :gen_server.call(:monster_templates, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:monster_templates, {:get, id})
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
    :gen_server.start_link({:local, :monster_templates}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
  end

  def handle_cast({:add, id, monster}, monster_templates) do
    {:noreply, HashDict.put_new(monster_templates, id, monster) }
  end

  def handle_cast({:remove, id}, monster_templates) do
    {:noreply, HashDict.delete(monster_templates, id) }
  end

  def handle_call(:all, _from, monster_templates) do
    {:reply, HashDict.values(monster_templates), monster_templates}
  end

  def handle_call({:get, id}, _from, monster_templates) do
    {:reply, HashDict.get(monster_templates, id), monster_templates}
  end

end