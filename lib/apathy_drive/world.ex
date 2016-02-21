defmodule ApathyDrive.World do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    :ets.new(:rooms, [:named_table, :set, read_concurrency: true])

    {:ok, nil}
  end

  def add_room(%Room{} = room) do
    GenServer.cast(__MODULE__, {:add_room, self, room})

    room
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

  def handle_cast({:add_room, pid, %Room{id: id} = room}, state) do
    :ets.insert(:rooms, {pid, room})
    :ets.insert(:rooms, {id, room})

    Process.monitor(pid)

    {:noreply, state}
  end

  def handle_info(message, state) do
    raise message

    {:noreply, state}
  end
end