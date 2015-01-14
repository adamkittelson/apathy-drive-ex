defmodule Utility do
  use Systems.Reload

  def send_message(entity, event, message \\ %{}) do
    observer = Possession.possessor(entity) || entity

    spirit = Spirit.value(observer)

    if match?(%Spirit{}, spirit) do
      Phoenix.Channel.reply spirit.socket, event, %{:html => message}
    end
  end

end
