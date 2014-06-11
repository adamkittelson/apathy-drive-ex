defmodule Abilities do
  use Systems.Reload
  use GenServer.Behaviour

  # Public API
  def add(ability_name, ability) do
    :gen_server.cast(:abilities, {:add, ability_name, ability})
  end

  def value do
    :gen_server.call(:abilities, :value)
  end

  def all do
    value |> Map.values
  end

  def find(key) when is_atom key do
    find(Atom.to_string(key))
  end

  def find(key) do
    ability = value[key]
    if ability do
      Components.Module.value(ability)
    end
  end

  # GenServer API
  def start_link do
    :gen_server.start_link({:local, :abilities}, __MODULE__, %{}, [])
  end

  def init(%{}) do
    {:ok, %{}}
  end

  def handle_cast({:add, ability_name, ability}, abilities) do
    {:noreply, Map.put(abilities, ability_name, ability)}
  end

  def handle_call(:value, _from, abilities) do
    {:reply, abilities, abilities}
  end

end
