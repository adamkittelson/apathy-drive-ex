defmodule Possession do
  use Systems.Reload
  use GenServer

  # Public API
  def possess(possessor, possessed) do
    GenServer.cast(:possession, {:possess, possessor, possessed})
  end

  def unpossess(possessor, possessed) do
    GenServer.cast(:possession, {:unpossess, possessor, possessed})
  end

  def possessor(possessed) do
    GenServer.call(:possession, {:possessor, possessed})
  end

  def possessed(possessor) do
    GenServer.call(:possession, {:possessed, possessor})
  end

  # GenServer API
  def start_link() do
    initial = %{:possessors => %{}, :possessions => %{}}
    initial = Enum.into(initial, HashDict.new)
    GenServer.start_link(__MODULE__, initial, name: :possession)
  end

  def init(parents) do
    {:ok, parents}
  end

  def handle_cast({:possess, possessor, possessed}, possessions) do
    possessions = possessions
                  |> put_in([:possessors, possessed], possessor)
                  |> put_in([:possessions, possessor], possessed)
    {:noreply, possessions }
  end

  def handle_cast({:unpossess, possessor, possessed}, possessions) do
    possessions = possessions
                  |> put_in([:possessors],  HashDict.delete(possessions[:possessors], possessed))
                  |> put_in([:possessions], HashDict.delete(possessions[:possessions], possessor))
    {:noreply, possessions }
  end

  def handle_call({:possessor, possessed}, _from, possessions) do
    {:reply, get_in(possessions, [:possessors, possessed]), possessions}
  end

  def handle_call({:possessed, possessor}, _from, possessions) do
    {:reply, get_in(possessions, [:possessions, possessor]), possessions}
  end

end
