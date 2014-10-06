defmodule Commands.Remove do
  use Systems.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    Systems.Item.unequip(monster, Enum.join(arguments, " "))
  end
end
