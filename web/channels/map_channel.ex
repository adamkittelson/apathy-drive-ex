defmodule ApathyDrive.MapChannel do
  use ApathyDrive.Web, :channel
  alias ApathyDrive.{Room, Repo}

  def join("map", %{}, socket) do
    send(self, :after_join)

    {:ok, socket}
  end

  def handle_info(:after_join, socket) do
    rooms =
      Room.world_map
      |> Repo.all
      |> Enum.reduce(%{}, fn map, rooms ->
           directions =
             map.exits
             |> Enum.filter(&(&1["kind"] in ["Normal", "Action", "Door", "Gate"]))
             |> Enum.map(&(&1["direction"]))

           map =
             map
             |> Map.put(:directions, directions)
             |> Map.delete(:exits)


           [%{"destination" => 6319, "direction" => "south", "kind" => "Normal"}, %{"destination" => 6317, "direction" => "southwest", "kind" => "Normal"}]
           Map.put(rooms, to_string(map.id), Map.delete(map, :id))
         end)

    push socket, "full_map", rooms
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
