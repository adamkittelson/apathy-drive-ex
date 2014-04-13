defmodule Rooms do
  use GenServer.Behaviour

  # Public API
  def add(room) do
    :gen_server.cast(:rooms, {:add, room})
  end

  def all do
    :gen_server.call(:rooms, :all)
  end

  def find_all_by_name(name) do
    Enum.filter(all, fn (room) ->
      room |> Components.Name.get_name
           |> String.downcase
           |> String.contains?(String.downcase(name))
    end)
  end

  # GenServer API
  def start_link() do
    :gen_server.start_link({:local, :rooms}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:add, room}, rooms) do
    {:noreply, [room | rooms] }
  end

  def handle_call(:all, _from, rooms) do
    {:reply, rooms, rooms}
  end

end