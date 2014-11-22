defmodule ApathyDrive.MUD do
  use Phoenix.Channel
  import Utility

  def join(socket, "mud", message) do
    Systems.Login.login(socket, message["login"])
    {:ok, socket}
  end

  def event(socket, "command", "") do
    event(socket, "command", "l")
  end

  def event(socket, "command", message) do
    [command | arguments] = String.split(message)

    character = Characters.find_by_socket(socket.pid)

    try do
      Systems.Command.execute(character, command, arguments)
    catch
      kind, error ->
        send_message(character, "scroll", "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts "Error while processing command: '#{command}' with arguments: #{inspect arguments}"
        if Entity.has_component?(character, Components.Name) do
          IO.puts "Character: #{Components.Name.value(character)}"
        end
        IO.puts Exception.format(kind, error)
    end

    socket
  end

  def leave(socket, _message) do
    character = Characters.find_by_socket(socket.pid)
    if character do
      Characters.remove_socket(socket.pid)
      Components.Socket.value(character, nil)
      Components.Online.value(character, false)
      Entities.save!(character)
    end
    socket
  end
end
