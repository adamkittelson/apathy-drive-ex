defmodule Commands.Inventory do
  use ApathyDrive.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, _arguments) do
    Systems.Item.display_inventory(monster)
  end
end
