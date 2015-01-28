defmodule Commands.Remove do
  use ApathyDrive.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    Systems.Item.unequip(monster, Enum.join(arguments, " "))
  end
end
