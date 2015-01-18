defmodule ApathyDrive.MUD do
  use Phoenix.Channel
  use Systems.Reload
  import Utility

  def join("mud", message, socket) do
    if spirit = Systems.Login.login(socket, message["login"]) do
      send_message(spirit, "clear scroll")
      Systems.Room.display_room_in_scroll(spirit, nil)
      Systems.Prompt.display(spirit, nil)

      socket = Phoenix.Socket.assign(socket, :spirit, spirit)

      {:ok, socket}
    else
      Phoenix.Channel.reply socket, "redirect", %{:url => "/"}
    end
  end

  def handle_in("new:msg", message, socket) do
      broadcast socket, "new:msg", message
      {:ok, socket}
    end

  def handle_in("command", "", socket) do
    handle_in("command", "l", socket)
  end

  def handle_in("command", message, socket) do
    [command | arguments] = String.split(message)

    spirit = socket.assigns[:spirit]

    try do
      Systems.Command.execute(spirit, command, arguments)
    catch
      kind, error ->
        send_message(spirit, "scroll", "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts "Error while processing command: '#{command}' with arguments: #{inspect arguments}"
        if name = Spirit.value(spirit).name do
          IO.puts "Spirit: #{name}"
        end
        IO.puts Exception.format(kind, error)
    end

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
