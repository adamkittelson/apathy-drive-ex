defmodule ApathyDrive.MUDChannel do
  use ApathyDrive.Web, :channel

  def join("mud:play", %{"spirit" => token}, socket) do
    case Phoenix.Token.verify(socket, "spirit", token, max_age: 1209600) do
      {:ok, spirit_id} ->
        case Repo.get!(Spirit, spirit_id) do
          nil ->
            {:error, %{reason: "unauthorized"}}
          %Spirit{} = spirit ->
            case Process.whereis(:"spirit_#{spirit.id}") do
              nil ->
                {:ok, pid} = ApathyDrive.Mobile.start(%ApathyDrive.Mobile{spirit: spirit.id, socket: self})
                socket = assign(socket, :mobile, pid)
                send(self, :after_join)
                {:ok, socket}
              pid ->
                socket = assign(socket, :mobile, pid)
                send(self, :after_join)
                {:ok, socket}
            end
        end
      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def handle_info({:update_mobile, pid}, socket) do
    socket = assign(socket, :mobile, pid)

    {:noreply, socket}
  end

  def handle_info({:respawn, spirit: spirit}, socket) do
    spirit = Repo.save!(spirit)

    {:ok, pid} = ApathyDrive.Mobile.start(%{spirit: spirit.id, socket: self})

    socket = assign(socket, :mobile, pid)

    ApathyDrive.Command.execute(socket.assigns[:mobile], "look", [])

    {:noreply, socket}
  end

  def handle_info(:after_join, socket) do
    send(socket.assigns[:mobile], {:set_socket, self})

    ApathyDrive.Command.execute(socket.assigns[:mobile], "look", [])

    {:noreply, socket}
  end

  def handle_info({:disable_element, elem}, socket) do
    Phoenix.Channel.push socket, "disable", %{:html => elem}

    {:noreply, socket}
  end

  def handle_info({:scroll, html}, socket) do
    Phoenix.Channel.push socket, "scroll", %{:html => html}

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

  def handle_info(:go_home, socket) do
    Phoenix.Channel.push socket, "redirect", %{:url => "/"}

    {:noreply, socket}
  end

  def handle_info(%Phoenix.Socket.Broadcast{event: event, payload: payload}, socket) do
    Phoenix.Channel.push socket, event, payload

    {:noreply, socket}
  end

  def handle_in("command", %{}, socket) do
    ApathyDrive.Command.execute(socket.assigns[:mobile], "l", [])

    {:noreply, socket}
  end

  def handle_in("command", message, socket) do
    case String.split(message) do
      [command | arguments] ->
        ApathyDrive.Command.execute(socket.assigns[:mobile], command, arguments)
      [] ->
        ApathyDrive.Command.execute(socket.assigns[:mobile], "l", [])
    end

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

end
