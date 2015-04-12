defmodule Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(%Spirit{} = spirit, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:gossip", {:gossip, name(spirit), message})
    spirit
  end

  def execute(%Monster{spirit: spirit} = monster, arguments) do
    message = sanitize(arguments)
    ApathyDrive.PubSub.broadcast!("chat:gossip", {:gossip, name(spirit), message})
    monster
  end

  def name(%Spirit{name: name, alignment: "good"}) do
    "<span class='white'>#{name}</span>"
  end

  def name(%Spirit{name: name, alignment: "neutral"}) do
    "<span class='dark-cyan'>#{name}</span>"
  end

  def name(%Spirit{name: name, alignment: "evil"}) do
    "<span class='magenta'>#{name}</span>"
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
