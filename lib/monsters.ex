defmodule Monsters do
  use GenServer.Behaviour

  # Public API
  def add(monster) do
    :gen_server.cast(:monsters, {:add, monster})
  end

  def all do
    :gen_server.call(:monsters, :all)
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
    :gen_server.start_link({:local, :monsters}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, monster}, monsters) do
    {:noreply, [monster | monsters] }
  end

  def handle_call(:all, _from, monsters) do
    {:reply, monsters, monsters}
  end

end