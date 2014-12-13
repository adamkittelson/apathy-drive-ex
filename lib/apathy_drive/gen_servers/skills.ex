defmodule Skills do
  use Systems.Reload
  use GenServer

  # Public API
  def add(skill_name, skill) do
    GenServer.cast(:skills, {:add, skill_name, skill})
  end

  def value do
    GenServer.call(:skills, :value)
  end

  def all do
    value |> Map.values
  end

  def find(key) when is_atom key do
    find(Atom.to_string(key))
  end

  def find(key) do
    skill = value[key]
    if skill do
      Components.Module.value(skill)
    else
       Skills.NotFound
    end
  end

  def universal do
    all |> Enum.filter(fn(skill) ->
             Components.Module.value(skill).universal?
           end)
        |> Enum.map(&Components.Name.value/1)
  end

  # GenServer API
  def start_link do
    GenServer.start_link(Skills, %{}, name: :skills)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add, skill_name, skill}, skills) do
    {:noreply, Map.put(skills, skill_name, skill) }
  end

  def handle_call(:value, _from, skills) do
    {:reply, skills, skills}
  end

end
