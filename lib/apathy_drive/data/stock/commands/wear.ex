defmodule Commands.Wear do
  use Systems.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    Systems.Item.equip(monster, Enum.join(arguments, " "))
  end
end
