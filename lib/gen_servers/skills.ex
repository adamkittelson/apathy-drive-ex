defmodule Skills do
  use GenServer.Behaviour

  # Public API
  def add(skill_name, skill) do
    :gen_server.cast(:skills, {:add, skill_name, skill})
  end

  def all do
    :gen_server.call(:skills, :all)
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

  def handle_call(:all, _from, skills) do
    {:reply, skills, skills}
  end

end
