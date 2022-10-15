defmodule ApathyDriveWeb.MUDChannel do
  use ApathyDriveWeb, :channel
  alias ApathyDrive.{ChannelHistory, Character, Mobile, RoomServer}
  require Logger

  def join("mud:play", %{"character" => token}, socket) do
    case Phoenix.Token.verify(socket, "character", token, max_age: 1_209_600) do
      {:ok, character_id} ->
        case Repo.get!(Character, character_id) do
          nil ->
            {:error, %{reason: "unauthorized"}}

          # Character has been reset, probably due to a game wipe
          %Character{name: nil} ->
            {:error, %{reason: "unauthorized"}}

          %Character{room_id: room_id} = character ->
            character =
              room_id
              |> RoomServer.find()
              |> RoomServer.character_connected(character, self())

            Logger.metadata(character: character.name)

            socket =
              socket
              |> assign(:room_id, room_id)
              |> assign(:character, character.id)
              |> assign(:power, Mobile.power(character))
              |> assign(:level, character.level)
              |> assign(:monster_ref, character.ref)
              |> assign(:commands, :queue.new())
              |> monitor_room()

            ApathyDrive.PubSub.subscribe("spirits:online")
            ApathyDrive.PubSub.subscribe("chat:gossip")
            ApathyDrive.PubSub.subscribe("chat:announce")

            send(self(), :after_join)

            {:ok, socket}
        end

      {:error, _error} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def handle_info({:DOWN, ref, :process, _object, _reason}, socket) do
    if ref == socket.assigns[:room_monitor_ref] do
      send_scroll(socket, "<p><span class='dark-red'>Something went wrong.</span></p>")

      character = Repo.get!(Character, socket.assigns[:character])

      character =
        socket.assigns[:room_id]
        |> RoomServer.find()
        |> RoomServer.character_connected(character, self())

      socket =
        socket
        |> assign(:monster_ref, character.ref)
        |> monitor_room()

      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  def handle_info(:after_join, socket) do
    socket.assigns[:room_id]
    |> RoomServer.find()
    |> RoomServer.enqueue_command(socket.assigns[:monster_ref], "l", [])

    [first | rest] = ChannelHistory.fetch(socket.assigns[:character], 100)

    rest
    |> Enum.reverse()
    |> Enum.each(fn %{message: message, time: time, channel: channel} ->
      Phoenix.Channel.push(socket, "chat-sidebar", %{
        html: message,
        time: Timex.from_now(time),
        chat_tab: chat_tab(channel)
      })
    end)

    Phoenix.Channel.push(socket, "chat-sidebar", %{
      html: first.message,
      time: Timex.from_now(first.time),
      force_time: true,
      chat_tab: chat_tab(first.channel)
    })

    {:noreply, socket}
  end

  def handle_info({:update_ref, ref}, socket) do
    socket = assign(socket, :monster_ref, ref)

    {:noreply, socket}
  end

  def handle_info({:disable_element, elem}, socket) do
    Phoenix.Channel.push(socket, "disable", %{:html => elem})

    {:noreply, socket}
  end

  def handle_info({:update_character, %{room_id: room_id, power: power, level: level}}, socket) do
    socket =
      socket
      |> assign(:room_id, room_id)
      |> assign(:power, power)
      |> assign(:level, level)
      |> monitor_room()

    update_room(socket)

    {:noreply, socket}
  end

  def handle_info({:update_score, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update score", data)

    {:noreply, socket}
  end

  def handle_info(:reconnect, socket) do
    {:stop, :reconnect, socket}
  end

  def handle_info(:show_talent_tree, socket) do
    Phoenix.Channel.push(socket, "show talents", %{})

    {:noreply, socket}
  end

  def handle_info({:update_energy_bar, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update energy bar", data)

    {:noreply, socket}
  end

  def handle_info({:update_attribute_bar, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update attribute bar", data)

    {:noreply, socket}
  end

  def handle_info({:update_exp_bar, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update exp bar", data)

    {:noreply, socket}
  end

  def handle_info({:update_mana_bar, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update mana bar", data)

    {:noreply, socket}
  end

  def handle_info({:update_hp_bar, %{} = data}, socket) do
    Phoenix.Channel.push(socket, "update hp bar", data)

    {:noreply, socket}
  end

  def handle_info({:update_moblist, {:safe, data}}, socket) do
    Phoenix.Channel.push(socket, "update mob list", %{html: to_string(data)})

    {:noreply, socket}
  end

  def handle_info({:pulse_score_attribute, attribute}, socket) do
    Phoenix.Channel.push(socket, "pulse score attribute", %{attribute: attribute})

    {:noreply, socket}
  end

  def handle_info({:scroll, %{} = data}, socket) do
    if socket.assigns[:spirit_id] in Map.keys(data) do
      send_scroll(socket, data[socket.assigns[:monster_ref]])
    else
      send_scroll(socket, data[:other])
    end

    {:noreply, socket}
  end

  def handle_info({:scroll, html}, socket) do
    send_scroll(socket, html)

    {:noreply, socket}
  end

  def handle_info({:chat, html}, socket) do
    Phoenix.Channel.push(socket, "chat", %{:html => html})

    {:noreply, socket}
  end

  def handle_info({:sidebar, html}, socket) do
    Phoenix.Channel.push(socket, "chat-sidebar", %{:html => html})

    {:noreply, socket}
  end

  def handle_info({:focus_element, elem}, socket) do
    Phoenix.Channel.push(socket, "focus", %{:html => elem})

    {:noreply, socket}
  end

  def handle_info(:up, socket) do
    Phoenix.Channel.push(socket, "up", %{})

    {:noreply, socket}
  end

  def handle_info({:update_prompt, html}, socket) do
    Phoenix.Channel.push(socket, "update prompt", %{:html => html})

    {:noreply, socket}
  end

  def handle_info(:go_home, socket) do
    Phoenix.Channel.push(socket, "redirect", %{:url => "/"})

    {:noreply, socket}
  end

  def handle_info({:open_tab, path}, socket) do
    Phoenix.Channel.push(socket, "open tab", %{:url => path})

    {:noreply, socket}
  end

  def handle_info(%Phoenix.Socket.Broadcast{event: event, payload: payload}, socket) do
    Phoenix.Channel.push(socket, event, payload)

    {:noreply, socket}
  end

  def handle_in("set_chat_tab", tab, socket) do
    socket.assigns[:room_id]
    |> RoomServer.find()
    |> RoomServer.set_chat_tab(socket.assigns[:monster_ref], tab)

    {:noreply, socket}
  end

  def handle_in("command", %{}, socket) do
    execute_command(socket, "l", [])

    {:noreply, socket}
  end

  def handle_in("command", message, socket) do
    case String.split(message) do
      [command | arguments] ->
        execute_command(socket, command, arguments)
        {:noreply, socket}

      [] ->
        execute_command(socket, "l", [])
        {:noreply, socket}
    end
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
    broadcast(socket, "shout", payload)
    {:noreply, socket}
  end

  # This is invoked every time a notification is being broadcast
  # to the client. The default implementation is just to push it
  # downstream but one could filter or change the event.
  def handle_out(event, payload, socket) do
    push(socket, event, payload)
    {:noreply, socket}
  end

  defp execute_command(socket, command, args) do
    Logger.info("command received: #{inspect({command, args})}")

    socket.assigns[:room_id]
    |> RoomServer.find()
    |> RoomServer.enqueue_command(socket.assigns[:monster_ref], command, args)
    |> case do
      :ok ->
        Logger.info("command enqueued #{inspect({command, args})}")

      :not_here ->
        Logger.info("command sent to wrong room!")
        # raise "Character sent command to a room they are not in"
    end
  end

  defp update_room(socket) do
    Phoenix.Channel.push(socket, "update_room", %{
      room_id: socket.assigns[:room_id],
      power: socket.assigns[:power],
      level: socket.assigns[:level]
    })
  end

  defp send_scroll(socket, html) do
    Phoenix.Channel.push(socket, "scroll", %{:html => html})
  end

  defp chat_tab("announce"), do: "announce"
  defp chat_tab(_), do: "chat"

  defp monitor_room(socket) do
    if ref = socket.assigns[:room_monitor_ref] do
      Process.demonitor(ref)
    end

    room_pid = RoomServer.find(socket.assigns[:room_id])

    assign(socket, :room_monitor_ref, Process.monitor(room_pid))
  end
end
