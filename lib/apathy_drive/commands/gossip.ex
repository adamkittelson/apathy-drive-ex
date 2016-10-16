defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Monster{} = monster, args) do
    message =
      args
      |> Enum.join(" ")
      |> Monster.sanitize()
    ApathyDrive.Endpoint.broadcast!("chat:gossip", "scroll", %{html: "<p>[<span class='dark-magenta'>gossip</span> : #{Monster.aligned_spirit_name(monster)}] #{message}</p>"})
    room
  end

end
