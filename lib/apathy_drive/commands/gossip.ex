defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(%Room{} = room, %Mobile{} = mobile, message) do
    message = Mobile.sanitize(message)
    ApathyDrive.Endpoint.broadcast!("chat:gossip", "scroll", %{html: "<p>[<span class='dark-magenta'>gossip</span> : #{Mobile.aligned_spirit_name(mobile)}] #{message}</p>"})
    room
  end

end
