defmodule ApathyDrive.Commands.Gossip do
  use ApathyDrive.Command

  def keywords, do: ["gos"]

  def execute(mobile, arguments) when is_pid(mobile) do
    Mobile.gossip(mobile, Enum.join(arguments, " "))
  end

  def execute(%Mobile{} = mobile, message) do
    message = Mobile.sanitize(message)
    ApathyDrive.PubSub.broadcast!("chat:gossip", {:gossip, Mobile.aligned_spirit_name(mobile), message})
  end

end
