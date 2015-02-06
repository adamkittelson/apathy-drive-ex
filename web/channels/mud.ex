defmodule ApathyDrive.MUD do
  use Phoenix.Channel
  use Systems.Reload
  import Utility

  def join("mud", message, socket) do
    if spirit = Systems.Login.login(socket, message["login"]) do
      send_message(spirit, "clear scroll")

      spirit_struct = Spirit.value(spirit)

      room = spirit_struct.room_id
             |> Room.find
             |> Room.value

      Room.look(room, spirit_struct)
      Systems.Prompt.display(spirit_struct)

      socket = Phoenix.Socket.assign(socket, :spirit, spirit)

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

    spirit = socket.assigns[:spirit]

    Spirit.execute_command(spirit, command, arguments)

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
