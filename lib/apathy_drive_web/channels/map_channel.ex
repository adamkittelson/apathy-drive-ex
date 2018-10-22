defmodule ApathyDriveWeb.MapChannel do
  use ApathyDriveWeb, :channel
  alias ApathyDrive.WorldMap

  def join("map", %{}, socket) do
    send(self(), :after_join)

    {:ok, socket}
  end

  def handle_info(:after_join, socket) do
    # Doing it in a task so it gets garbage collected right away
    Task.async(fn ->
      map = WorldMap.fetch()

      if map_size(map) > 0,
        do: push(socket, "update_map", map)
    end)
    |> Task.await()

    push(socket, "request_room_id", %{})

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
    push(socket, event, payload)
    {:noreply, socket}
  end
end
