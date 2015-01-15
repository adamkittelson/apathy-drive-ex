defmodule ApathyDrive.MUD do
  use Phoenix.Channel
  use Systems.Reload
  import Utility

  def join(socket, "mud", message) do
    if spirit = Systems.Login.login(socket, message["login"]) do
      send_message(spirit, "clear scroll")
      Systems.Room.display_room_in_scroll(spirit, nil, Parent.of(spirit))
      Systems.Prompt.display(spirit, nil)

      socket = Phoenix.Socket.assign(socket, :spirit, spirit)

      {:ok, socket}
    else
      Phoenix.Channel.reply socket, "redirect", %{:url => "/"}
    end
  end

  def event(socket, "command", "") do
    event(socket, "command", "l")
  end

  def event(socket, "command", message) do
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

    socket
  end

  def leave(socket, _message) do
    spirit = socket.assigns[:spirit]
    if spirit do
      spirit
      |> Spirit.value
      |> Spirit.save
      |> Spirits.remove
    end
    socket
  end
end
