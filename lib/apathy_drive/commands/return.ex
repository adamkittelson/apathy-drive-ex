defmodule ApathyDrive.Commands.Return do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["return"]

  def execute(mobile) when is_pid(mobile) do
    Mobile.teleport(mobile, :home)
  end

end
