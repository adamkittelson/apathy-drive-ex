defmodule ApathyDrive.MUD do
  use Phoenix.Channel

  def join("mud", %{"spirit" => id}, socket) do
    Process.flag(:trap_exit, true)

    send(self, {:login, id})

    {:ok, socket}
  end

  def handle_info({:login, id}, socket) do

    spirit = socket.assigns[:entity]

    if spirit = Systems.Login.login(socket, self, id) do

      socket = Phoenix.Socket.assign(socket, :entity, spirit)

      room = spirit.room_id
             |> Room.find
             |> Room.value

      Room.look(room, spirit)
      Systems.Prompt.display(spirit)
    else
      Phoenix.Channel.push socket, "redirect", %{:url => "/"}
    end

    {:noreply, socket}
  end

  def handle_info({:set_entity, entity}, socket) do
    {:noreply, Phoenix.Socket.assign(socket, :entity, entity)}
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

    socket = Phoenix.Socket.assign(socket, :entity, entity)

    {:noreply, socket}
  end

  def terminate(reason, socket) do
    case socket.assigns[:entity] do
      %Spirit{} = spirit ->
        Spirit.logout(spirit)
      %Monster{spirit: %Spirit{} = spirit} ->
        Spirit.save(spirit)
    end

    {:noreply, socket}
  end
end
