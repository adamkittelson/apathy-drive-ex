defmodule Commands.Inventory do
  use Systems.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, _arguments) do
    Systems.Item.display_inventory(monster)
  end
end
