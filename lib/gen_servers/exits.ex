defmodule Exits do
  use GenServer.Behaviour

  # Public API
  def add(exit_pid) do
    id = Components.ID.value(exit_pid)
    :gen_server.cast(:exits, {:add, id, exit_pid})
  end

  def remove(exit_pid) do
    id = Components.ID.value(exit_pid)
    :gen_server.cast(:exits, {:remove, id})
  end

  def all do
    :gen_server.call(:exits, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:exits, {:get, id})
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
    :gen_server.start_link({:local, :exits}, __MODULE__, [], [])
  end

  def init(_) do
    {:ok, HashDict.new}
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
