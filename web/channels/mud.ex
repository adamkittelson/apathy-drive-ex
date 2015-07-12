defmodule ApathyDrive.MUD do
  use Phoenix.Channel

  def join("mud", %{"spirit" => id}, socket) do
    send(self, {:login, id})

    {:ok, socket}
  end

  def handle_info({:login, id}, socket) do

    if spirit = Systems.Login.login(socket, self, id) do

      socket = assign(socket, :entity, spirit)

      case spirit do
        %Spirit{} = spirit ->
          ApathyDrive.Endpoint.broadcast_from! spirit.pid, "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just entered the Realm.</p>"}
        %Monster{} = spirit ->
          ApathyDrive.Endpoint.broadcast_from! spirit.pid, "spirits:online", "scroll", %{:html => "<p>#{spirit.spirit.name} just entered the Realm.</p>"}
      end

      room = spirit.room_id
             |> Room.find
             |> Room.value

      Room.look(room, spirit)

      send(spirit.pid, :display_prompt)

      {:noreply, socket}
    else
      Phoenix.Channel.push socket, "redirect", %{:url => "/"}
      {:noreply, socket}
    end

  end

  def handle_info({:set_entity, entity}, socket) do
    {:noreply, assign(socket, :entity, entity)}
  end

  def handle_info(:go_home, socket) do
    Phoenix.Channel.push socket, "redirect", %{:url => "/"}

    {:noreply, assign(socket, :entity, nil)}
  end

  def handle_in("command", %{}, socket) do
    handle_in("command", "l", socket)
  end

  def handle_in("command", "", socket) do
    handle_in("command", "l", socket)
  end

  def handle_in("command", message, socket) do
    [command | arguments] = String.split(message)

    entity = case socket.assigns[:entity] do
      %Spirit{} = spirit ->
        Spirit.execute_command(spirit, command, arguments)
      %Monster{} = monster ->
        Monster.execute_command(monster, command, arguments)
    end

    socket = assign(socket, :entity, entity)

    {:noreply, socket}
  end

  def terminate(_reason, socket) do
    case socket.assigns[:entity] do
      %Spirit{} = spirit ->
        ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just left the Realm.</p>"}
        Spirit.logout(spirit)
      %Monster{spirit: %Spirit{} = spirit} = monster ->
        ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just left the Realm.</p>"}
        Monster.execute_command(monster, "unpossess", [])
      nil ->
        nil
    end

    {:noreply, socket}
  end
end
