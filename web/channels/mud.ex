defmodule ApathyDrive.MUD do
  use Phoenix.Channel

  def join("mud", %{"spirit" => id}, socket) do
    if spirit = Systems.Login.login(socket, id) do

      room = spirit.room_id
             |> Room.find
             |> Room.value

      Room.look(room, spirit)
      Systems.Prompt.display(spirit)

      socket = Phoenix.Socket.assign(socket, :entity, spirit)

      {:ok, socket}
    else
      Phoenix.Channel.reply socket, "redirect", %{:url => "/"}
    end
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

    {:ok, socket}
  end

  def leave(_message, socket) do
    spirit = socket.assigns[:spirit]
    if spirit do
      Spirit.logout(spirit)
    end
    {:ok, socket}
  end
end
