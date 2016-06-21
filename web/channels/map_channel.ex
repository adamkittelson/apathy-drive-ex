defmodule ApathyDrive.MapChannel do
  use ApathyDrive.Web, :channel
  alias ApathyDrive.{Room, Repo}

  def join("map", %{}, socket) do
    send(self, :after_join)

    {:ok, socket}
  end

  def handle_info(:after_join, socket) do
    map =
      Room.world_map
      |> Repo.all
      |> Enum.reduce(%{}, fn %{id: id} = room, map ->
           directions =
             room.exits
             |> Enum.filter(&(&1["kind"] in ["Normal", "Action", "Door", "Gate"]))
             |> Enum.map(&(&1["direction"]))

           room =
             room
             |> Map.put(:directions, directions)
             |> Map.delete(:exits)

           Map.put(map, to_string(id), room)
         end)

    push socket, "update_map", map

    {:noreply, socket}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # This is invoked every time a notification is being broadcast
  # to the client. The default implementation is just to push it
  # downstream but one could filter or change the event.
  def handle_out(event, payload, socket) do
    push socket, event, payload
    {:noreply, socket}
  end

end
