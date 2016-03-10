defmodule ApathyDrive.World do
  use GenServer
  alias ApathyDrive.Mobile

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    :ets.new(:rooms,   [:named_table, :set])
    :ets.new(:mobiles, [:named_table, :set])
    :ets.new(:unities, [:named_table, :set])

    {:ok, nil}
  end

  def add_room(%Room{} = room) do
    GenServer.cast(__MODULE__, {:add_room, self, room})

    room
  end

  def add_mobile(%Mobile{} = mobile) do
    GenServer.cast(__MODULE__, {:add_mobile, self, mobile})

    mobile
  end

  def set_average_essence(unity, essence) do
    GenServer.cast(__MODULE__, {:set_average_essence, unity, essence})
  end

  def average_essence(unity) do
    case :ets.lookup(:unities, unity) do
      [{^unity, essence}] ->
        essence
      _ ->
        nil
    end
  end

  def room(pid_or_id, retries \\ 0)
  def room(id, retries) when is_integer(id) do
    id
    |> Room.find
    |> room(retries)
  end
  def room(pid, retries) when retries < 5 do
    case :ets.lookup(:rooms, pid) do
      [{^pid, room}] ->
        room
      _ ->
        :timer.sleep(10)
        room(pid, retries + 1)
    end
  end
  def room(pid, _retries), do: raise "wtf couldn't find state for room #{inspect pid}"

  def mobile(pid, retries \\ 0)
  def mobile(pid, retries) when retries < 5 and is_pid(pid) do
    case :ets.lookup(:mobiles, pid) do
      [{^pid, mobile}] ->
        mobile
      _ ->
        :timer.sleep(10)
        mobile(pid, retries + 1)
    end
  end
  def mobile(pid, _retries) when is_pid(pid), do: raise "wtf couldn't find state for mobile #{inspect pid}"

  def handle_cast({:add_room, pid, %Room{id: id} = room}, state) do
    :ets.insert(:rooms, {id, room})

    case :ets.lookup(:rooms, pid) do
      [{_pid, %Room{id: _id}}] ->
        :ets.insert(:rooms, {pid, room})
      _ ->
        Process.monitor(pid)
        :ets.insert(:rooms, {pid, room})
    end

    {:noreply, state}
  end

  def handle_cast({:set_average_essence, unity, essence}, state) do
    :ets.insert(:unities, {unity, essence})

    {:noreply, state}
  end

  def handle_cast({:add_mobile, pid, %Mobile{id: id} = mobile}, state) do
    :ets.insert(:mobiles, {id, mobile})

    case :ets.lookup(:mobiles, pid) do
      [{_pid, %Mobile{id: _id}}] ->
        :ets.insert(:mobiles, {pid, mobile})
      _ ->
        Process.monitor(pid)
        :ets.insert(:mobiles, {pid, mobile})
    end

    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    case :ets.lookup(:rooms, pid) do
      [{_pid, %Room{id: id}}] ->
        :ets.delete(:rooms, pid)
        :ets.delete(:rooms, id)
      _ ->
        case :ets.lookup(:mobiles, pid) do
          [{_pid, %Mobile{id: id}}] ->
            :ets.delete(:mobiles, pid)
            :ets.delete(:mobiles, id)
          _ ->
            :noop
        end
    end

    {:noreply, state}
  end
end