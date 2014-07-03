defmodule Abilities do
  use Systems.Reload
  use GenServer

  # Public API
  def add(ability_name, ability) do
    GenServer.cast(:abilities, {:add, ability_name, ability})
  end

  def value do
    GenServer.call(:abilities, :value)
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

  def find_by_module(module) do
    Enum.find(all, fn (ability) ->
      Components.Module.value(ability) == module
    end)
  end

  # GenServer API
  def start_link do
    GenServer.start_link(Abilities, %{}, name: :abilities)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add, ability_name, ability}, abilities) do
    {:noreply, Map.put(abilities, ability_name, ability)}
  end

  def handle_call(:value, _from, abilities) do
    {:reply, abilities, abilities}
  end

end
