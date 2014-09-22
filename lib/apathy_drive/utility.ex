defmodule Utility do
  use Systems.Reload

  def send_message(entity, event, message \\ %{}) do
    entity = Possession.possessor(entity) || entity
    if Entity.has_component?(entity, Components.Socket) do
      socket = Components.Socket.value(entity)
      if socket do
        IO.puts "socket: #{inspect socket}"
        IO.puts "event: #{inspect event}"
        IO.puts "message: #{inspect message}"
        Phoenix.Channel.reply socket, event, %{:html => message}
      end
    end
  end

end
