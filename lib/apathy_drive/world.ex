defmodule ApathyDrive.World do
  use GenServer
  alias ApathyDrive.Mobile

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    :ets.new(:rooms,   [:named_table, :set, read_concurrency: true])
    :ets.new(:mobiles, [:named_table, :set, read_concurrency: true])

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

  def room(pid_or_id) do
    case :ets.lookup(:rooms, pid_or_id) do
      [{^pid_or_id, room}] ->
        room
      _ when is_pid(pid_or_id) ->
        room(pid_or_id)
      _ when is_integer(pid_or_id) ->
        Room.find(pid_or_id)
        |> room()
    end
  end

  def mobile(pid_or_id) do
    case :ets.lookup(:mobiles, pid_or_id) do
      [{^pid_or_id, mobile}] ->
        mobile
      _ ->
        mobile(pid_or_id)
    end
  end

  def handle_cast({:add_room, pid, %Room{id: id} = room}, state) do
    :ets.insert(:rooms, {id, room})

    case :ets.lookup(:rooms, pid) do
      [{_pid, %Mobile{id: id}}] ->
        :ets.insert(:rooms, {pid, room})
      _ ->
        Process.monitor(pid)
        :ets.insert(:rooms, {pid, room})
    end

    {:noreply, state}
  end

  def handle_cast({:add_mobile, pid, %Mobile{id: id} = mobile}, state) do
    :ets.insert(:mobiles, {id, mobile})

    case :ets.lookup(:mobiles, pid) do
      [{_pid, %Mobile{id: id}}] ->
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