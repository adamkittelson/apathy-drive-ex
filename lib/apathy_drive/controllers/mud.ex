defmodule ApathyDrive.MUD do
  use Phoenix.Channel
  import Utility

  def join(socket, "mud", message) do
    Systems.Login.login(socket, message["login"])
    {:ok, socket}
  end

  def event(socket, "command", message) do
    [command | arguments] = String.split(message)

    character = Characters.find_by_socket(socket)

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

  def leave(socket, message) do
    character = Characters.find_by_socket(socket)
    Components.Socket.value(character, nil)
    Components.Online.value(character, false)
    socket
  end
end
