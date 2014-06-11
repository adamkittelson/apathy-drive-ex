defmodule Skills do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(skill_name, skill) do
    :gen_server.cast(:skills, {:add, skill_name, skill})
  end

  def value do
    :gen_server.call(:skills, :value)
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

  # GenServer API
  def start_link do
    :gen_server.start_link({:local, :skills}, __MODULE__, %{}, [])
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
