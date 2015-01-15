defmodule Spirits do
  use Systems.Reload
  use GenServer

  # Public API
  def add(spirit) do
    GenServer.call(:spirits, {:add, spirit})
  end

  def remove(spirit) do
    GenServer.cast(:spirits, {:remove, spirit})
  end

  def all do
    GenServer.call(:spirits, :all)
  end

  def get(id) do
    GenServer.call(:characters, {:get, id})
  end

  def find_by_url(url) do
    GenServer.call(:characters, {:url, url})
  end

  # GenServer API
  def start_link do
    GenServer.start_link(Spirits, HashDict.new, name: :spirits)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_call({:add, spirit}, _from, spirits) do
    {:ok, spirit_pid} = Supervisor.start_child(:spirit_supervisor, {:"spirit_#{spirit.id}", {GenServer, :start_link, [Spirit, spirit]}, :permanent, 5000, :worker, [Spirit]})

    {:reply, spirit_pid, HashDict.put_new(spirits, spirit.id, spirit_pid)}
  end

  def handle_call(:all, _from, spirits) do
    {:reply, HashDict.values(spirits), spirits}
  end

  def handle_cast({:remove, spirit}, spirits) do
    spirit_to_kill = :"spirit_#{spirit.id}"
    Supervisor.terminate_child(:spirit_supervisor, spirit_to_kill)
    Supervisor.delete_child(:spirit_supervisor, spirit_to_kill)
    {:noreply, HashDict.delete(spirits, spirit.id)}
  end



end
