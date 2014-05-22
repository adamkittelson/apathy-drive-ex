defmodule Rooms do
  use GenServer.Behaviour

  # Public API
  def add(room) do
    :gen_server.cast(:rooms, {:add, room})
  end

  def all do
    :gen_server.call(:rooms, :all)
  end

  def find_by_id(id) do
    :gen_server.call(:rooms, {:get, id})
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

  def init(_) do
    {:ok, HashDict.new}
  end

  def handle_cast({:add, room}, rooms) do
    id = Components.ID.value(room)
    {:noreply, HashDict.put_new(rooms, id, room) }
  end

  def handle_cast({:remove, room}, rooms) do
    id = Components.ID.value(room)
    {:noreply, HashDict.delete(rooms, id) }
  end

  def handle_call(:all, _from, rooms) do
    {:reply, HashDict.values(rooms), rooms}
  end

  def handle_call({:get, id}, _from, rooms) do
    {:reply, HashDict.get(rooms, id), rooms}
  end

end
