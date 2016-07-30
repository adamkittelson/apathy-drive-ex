defmodule ApathyDrive.MUDChannel do
  use ApathyDrive.Web, :channel
  alias ApathyDrive.RoomServer

  def join("mud:play", %{"spirit" => token}, socket) do
    case Phoenix.Token.verify(socket, "spirit", token, max_age: 1209600) do
      {:ok, spirit_id} ->
        case Repo.get!(Spirit, spirit_id) do
          nil ->
            {:error, %{reason: "unauthorized"}}
          %Spirit{name: nil} -> # spirit has been reset, probably due to a game wipe
            {:error, %{reason: "unauthorized"}}
          %Spirit{room_id: room_id} = spirit ->
            spirit =
              Repo.preload(spirit, :class)

            ref =
              room_id
              |> RoomServer.find
              |> RoomServer.spirit_connected(spirit, self())

            socket =
              socket
              |> assign(:room_id, room_id)
              |> assign(:spirit_id, spirit.id)
              |> assign(:mobile_ref, ref)

              ApathyDrive.PubSub.subscribe("chat:gossip")
              ApathyDrive.PubSub.subscribe("chat:#{String.downcase(spirit.class.name)}")

            send(self(), :after_join)

            {:ok, socket}
        end
      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def handle_info(:after_join, socket) do
    socket.assigns[:room_id]
    |> RoomServer.find
    |> RoomServer.execute_command(socket.assigns[:mobile_ref], "l", [])

    update_room(socket)

    {:noreply, socket}
  end

  def handle_info({:update_ref, ref}, socket) do
    socket = assign(socket, :mobile_ref, ref)

    {:noreply, socket}
  end

  def handle_info({:respawn, spirit: spirit}, socket) do
    spirit =
      update_in(spirit.experience, &(max(&1, 0)))
      |> Repo.save!

    ref =
      spirit.room_id
      |> RoomServer.find
      |> RoomServer.spirit_connected(spirit, self())

    socket =
      socket
      |> assign(:room_id, spirit.room_id)
      |> assign(:mobile_ref, ref)

    send(self(), :after_join)

    {:noreply, socket}
  end

  def handle_info({:disable_element, elem}, socket) do
    Phoenix.Channel.push socket, "disable", %{:html => elem}

    {:noreply, socket}
  end

  def handle_info({:update_room, room_id}, socket) do
    socket =
      socket
      |> assign(:room_id, room_id)

    update_room(socket)

    {:noreply, socket}
  end

  def handle_info({:scroll, %{} = data}, socket) do
    if socket.assigns[:spirit_id] in Map.keys(data) do
      send_scroll(socket, data[socket.assigns[:mobile_ref]])
    else
      send_scroll(socket, data[:other])
    end

    {:noreply, socket}
  end

  def handle_info({:scroll, html}, socket) do
    send_scroll(socket, html)

    {:noreply, socket}
  end

  def handle_info({:focus_element, elem}, socket) do
    Phoenix.Channel.push socket, "focus", %{:html => elem}

    {:noreply, socket}
  end

  def handle_info(:up, socket) do
    Phoenix.Channel.push socket, "up", %{}

    {:noreply, socket}
  end

  def handle_info({:update_prompt, html}, socket) do
    Phoenix.Channel.push socket, "update prompt", %{:html => html}

    {:noreply, socket}
  end

  def handle_info({:update_room_essence, essence}, socket) do
    Phoenix.Channel.push socket, "update room essence", essence

    {:noreply, socket}
  end

  def handle_info(:go_home, socket) do
    Phoenix.Channel.push socket, "redirect", %{:url => "/"}

    {:noreply, socket}
  end

  def handle_info({:open_tab, path}, socket) do
    Phoenix.Channel.push socket, "open tab", %{:url => path}

    {:noreply, socket}
  end

  def handle_info(%Phoenix.Socket.Broadcast{event: event, payload: payload}, socket) do
    Phoenix.Channel.push socket, event, payload

    {:noreply, socket}
  end

  def handle_in("command", %{}, socket) do

    socket.assigns[:room_id]
    |> RoomServer.find
    |> RoomServer.execute_command(socket.assigns[:mobile_ref], "l", [])

    {:noreply, socket}
  end

  def handle_in("command", message, socket) do
    case String.split(message) do
      [command | arguments] ->
        socket.assigns[:room_id]
        |> RoomServer.find
        |> RoomServer.execute_command(socket.assigns[:mobile_ref], command, arguments)
      [] ->
        socket.assigns[:room_id]
        |> RoomServer.find
        |> RoomServer.execute_command(socket.assigns[:mobile_ref], "l", [])
    end

    {:noreply, socket}
  end

  def handle_in("map", "request_room_id", socket) do
    update_room(socket)
    {:noreply, socket}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (mud:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # This is invoked every time a notification is being broadcast
  # to the client. The default implementation is just to push it
  # downstream but one could filter or change the event.
  def handle_out(event, payload, socket) do
    push socket, event, payload
    {:noreply, socket}
  end

  defp update_room(socket) do
    Phoenix.Channel.push socket, "update_room", %{:room_id => socket.assigns[:room_id]}
  end

  defp send_scroll(socket, html) do
    Phoenix.Channel.push socket, "scroll", %{:html => html}
  end

end
