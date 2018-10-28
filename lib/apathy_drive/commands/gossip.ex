defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command
  alias ApathyDrive.{ChannelHistory, Repo}

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Character{} = character, args) do
    raw_message = Enum.join(args, " ")

    message =
      "<p>[<span class='dark-magenta'>gossip</span> : #{character.name}] #{
        Character.sanitize(raw_message)
      }</p>"

    Repo.insert!(%ChannelHistory{
      message: message,
      channel_name: "gossip"
    })

    ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "chat", %{
      html: message
    })

    Gossip.broadcast("gossip", %{name: character.name, message: raw_message})
    room
  end
end
