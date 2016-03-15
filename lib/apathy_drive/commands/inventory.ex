defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(mobile, _arguments) do
    Mobile.display_inventory(mobile)
  end
end