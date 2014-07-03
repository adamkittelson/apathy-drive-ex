defmodule Exits do
  use Systems.Reload
  use GenServer

  # Public API
  def add(exit_pid) do
    id = Components.ID.value(exit_pid)
    GenServer.cast(:exits, {:add, id, exit_pid})
  end

  def remove(exit_pid) do
    id = Components.ID.value(exit_pid)
    GenServer.cast(:exits, {:remove, id})
  end

  def all do
    GenServer.call(:exits, :all)
  end

  def find_by_id(id) do
    GenServer.call(:exits, {:get, id})
  end

  def find_all_by_name(name) do
    Enum.filter(all, fn (exit_pid) ->
      exit_pid |> Components.Name.get_name
           |> String.downcase
           |> String.contains?(String.downcase(name))
    end)
  end

  # GenServer API
  def start_link() do
    GenServer.start_link(Exits, HashDict.new, name: :exits)
  end

  def init(exits) do
    {:ok, exits}
  end

  def handle_cast({:add, id, exit_pid}, exits) do
    {:noreply, HashDict.put_new(exits, id, exit_pid) }
  end

  def handle_cast({:remove, id}, exits) do
    {:noreply, HashDict.delete(exits, id) }
  end

  def handle_call(:all, _from, exits) do
    {:reply, HashDict.values(exits), exits}
  end

  def handle_call({:get, id}, _from, exits) do
    {:reply, HashDict.get(exits, id), exits}
  end

end
