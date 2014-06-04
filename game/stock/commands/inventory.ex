defmodule Commands.Inventory do
  use Systems.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(entity, _arguments) do
    Systems.Item.display_inventory(entity)
  end
end
