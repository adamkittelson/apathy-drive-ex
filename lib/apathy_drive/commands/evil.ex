defmodule Commands.Evil do
  use ApathyDrive.Command

  def keywords, do: ["evil"]

  def execute(%Spirit{alignment: "evil"} = spirit, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:evil", {:evil, spirit.name, message})
    spirit
  end

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p>You are not <span class='magenta'>Evil</span>!</p>")
  end

  def execute(%Monster{spirit: %Spirit{alignment: "evil"} = spirit} = monster, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:evil", {:evil, spirit.name, message})
    monster
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p>You are not <span class='magenta'>Evil</span>!</p>")
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
