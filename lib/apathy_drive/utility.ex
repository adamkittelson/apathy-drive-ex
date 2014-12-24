defmodule Utility do
  use Systems.Reload

  def send_message(entity, event, message \\ %{}) do
    observer = Possession.possessor(entity) || entity
    if observer && Process.alive?(observer) do
      if Entity.has_component?(observer, Components.Socket) do
        socket = Components.Socket.value(observer)
        if socket do
          Phoenix.Channel.reply socket, event, %{:html => message}
        end
      end
    end
  end

end
