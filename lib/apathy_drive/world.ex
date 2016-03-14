defmodule ApathyDrive.World do
  use GenServer
  alias ApathyDrive.{Mobile, RoomUnity}

  def start_link do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    :ets.new(:unities, [:named_table, :set])

    send(self, :load)

    {:ok, nil}
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

  def handle_cast({:set_average_essence, unity, essence}, state) do
    :ets.insert(:unities, {unity, essence})

    {:noreply, state}
  end

  def handle_info(:load, state) do
    Task.start fn ->
      load_mobiles()
      load_rooms()
    end
    {:noreply, state}
  end

  def load_mobiles do
    Mobile.ids
    |> ApathyDrive.Repo.all
    |> Enum.each(fn(%{id: id, room_id: room_id}) ->
         Mobile.load(id)
         Room.find(room_id)
       end)
  end

  def load_rooms do
    RoomUnity.room_ids
    |> ApathyDrive.Repo.all
    |> Enum.each(&Room.find/1)
  end
end
