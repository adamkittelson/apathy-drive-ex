defmodule CritTables do
  use Systems.Reload
  use GenServer

  # Public API
  def add(table_name, table) do
    GenServer.cast(:crit_tables, {:add, table_name, table})
  end

  def value do
    GenServer.call(:crit_tables, :value)
  end

  def damage_types do
    value
    |> Map.keys
    |> Enum.reduce(%{}, fn(table, map) ->
         put_in map[table], value[table].damage_type
       end)
  end

  def find(key) when is_atom key do
    find(Atom.to_string(key))
  end

  def find(key) do
    value[key]
  end

  # GenServer API
  def start_link do
    GenServer.start_link(CritTables, %{}, name: :crit_tables)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add, table_name, table}, tables) do
    {:noreply, Map.put(tables, table_name, table) }
  end

  def handle_call(:value, _from, tables) do
    {:reply, tables, tables}
  end

end
