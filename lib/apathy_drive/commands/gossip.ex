defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(mobile, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:gossip", {:gossip, Mobile.aligned_spirit_name(mobile), message})
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
