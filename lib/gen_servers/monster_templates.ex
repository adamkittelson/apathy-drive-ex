defmodule MonsterTemplates do
  use GenServer.Behaviour

  # Public API
  def add(monster) do
    :gen_server.cast(:monster_templates, {:add, monster})
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

  def handle_cast({:add, monster}, monster_templates) do
    id = Components.ID.value(monster)
    {:noreply, HashDict.put_new(monster_templates, id, monster) }
  end

  def handle_call(:all, _from, monster_templates) do
    {:reply, HashDict.values(monster_templates), monster_templates}
  end

  def handle_call({:get, id}, _from, monster_templates) do
    {:reply, HashDict.get(monster_templates, id), monster_templates}
  end

end