defmodule Commands.Angel do
  use ApathyDrive.Command

  def keywords, do: ["angel"]

  def execute(%Spirit{faction: "Angel"} = spirit, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:Angel", {:angel, spirit.name, message})
    spirit
  end

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p>You are not an <span class='white'>Angel</span>!</p>")
  end

  def execute(%Monster{spirit: %Spirit{faction: "Angel"} = spirit} = monster, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:Angel", {:angel, spirit.name, message})
    monster
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p>You are not an <span class='white'>Angel</span>!</p>")
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
