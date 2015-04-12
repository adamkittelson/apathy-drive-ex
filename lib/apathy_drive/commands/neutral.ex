defmodule Commands.Neutral do
  use ApathyDrive.Command

  def keywords, do: ["balance"]

  def execute(%Spirit{alignment: "neutral"} = spirit, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:neutral", {:neutral, spirit.name, message})
    spirit
  end

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p>You are not <span class='dark-cyan'>Neutral</span>!</p>")
  end

  def execute(%Monster{spirit: %Spirit{alignment: "neutral"} = spirit} = monster, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:neutral", {:neutral, spirit.name, message})
    monster
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p>You are not <span class='dark-cyan'>Neutral</span>!</p>")
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
