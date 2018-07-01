defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command
  alias ApathyDrive.Gossip

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Character{} = character, args) do
    message =
      args
      |> Enum.join(" ")
      |> Character.sanitize()

    ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "scroll", %{
      html: "<p>[<span class='dark-magenta'>gossip</span> : #{character.name}] #{message}</p>"
    })

    Gossip.broadcast("gossip", character.name, message)
    room
  end
end
