defmodule Rooms do
  use Systems.Reload
  use GenServer

  # Public API
  def add(room) do
    id = Components.ID.value(room)
    GenServer.cast(:rooms, {:add, id, room})
  end

  def all do
    GenServer.call(:rooms, :all)
  end

  def find_by_id(id) do
    GenServer.call(:rooms, {:get, id})
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
    GenServer.start_link(Rooms, HashDict.new, name: :rooms)
  end

  def init(rooms) do
    {:ok, rooms}
  end

  def handle_cast({:add, id, room}, rooms) do
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
    if room = HashDict.get(rooms, id) do
      {:reply, room, rooms}
    else
      {:ok, room} = Room.load(id)
      {:reply, room, HashDict.put_new(rooms, id, room)}
    end
  end

end
