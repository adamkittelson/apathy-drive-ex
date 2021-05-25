defmodule ApathyDrive.WorldMap do
  use GenServer
  require Logger
  alias ApathyDrive.{Repo, Room}

  def start_link(_arg) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_state) do
    send(self(), :load_world_map)

    {:ok, %{}}
  end

  def fetch do
    GenServer.call(__MODULE__, :fetch)
  end

  def handle_call(:fetch, _from, map) do
    {:reply, map, map}
  end

  def handle_info(:load_world_map, _map) do
    map =
      Room.world_map()
      |> Repo.all()
      |> Enum.reduce(%{}, fn %{level: level, map: map, name: area_name}, world_map ->
        if map_size(map) > 0 do
          Map.put(world_map, area_name, %{level: level, rooms: map})
        else
          world_map
        end
      end)

    Logger.info("map loaded")

    ApathyDriveWeb.Endpoint.broadcast!("map", "update_map", map)
    ApathyDriveWeb.Endpoint.broadcast!("map", "request_room_id", %{})
    {:noreply, map}
  end
end
