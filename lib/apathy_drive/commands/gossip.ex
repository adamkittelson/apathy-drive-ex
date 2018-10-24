defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Character{} = character, args) do
    message = Enum.join(args, " ")

    ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "chat", %{
      html:
        "<p>[<span class='dark-magenta'>gossip</span> : #{character.name}] #{
          Character.sanitize(message)
        }</p>"
    })

    Gossip.broadcast("gossip", %{name: character.name, message: message})
    room
  end
end
