defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Character{} = character, args) do
    message =
      args
      |> Enum.join(" ")
      |> Character.sanitize()
    ApathyDrive.Endpoint.broadcast!("chat:gossip", "scroll", %{html: "<p>[<span class='dark-magenta'>gossip</span> : #{character.name}] #{message}</p>"})
    room
  end

end
