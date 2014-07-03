defmodule ApathyDrive.MUD do
  use Phoenix.Channel

  def join(socket, "mud", message) do
    Systems.Login.login(socket, message["login"])
    {:ok, socket}
  end

  def event(socket, "command", message) do
    [command | arguments] = String.split(message)

    character = Characters.find_by_socket(socket)

    Systems.Command.execute(character, command, arguments)
    socket
  end

  def leave(socket, message) do
    character = Characters.find_by_socket(socket)
    Components.Socket.value(character, nil)
    Components.Online.value(character, false)
    socket
  end
end
