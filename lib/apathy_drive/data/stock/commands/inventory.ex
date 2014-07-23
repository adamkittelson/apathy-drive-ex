defmodule Commands.Inventory do
  use Systems.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(entity, _arguments) do
    if Components.Spirit.value(entity) do
      send_message(entity, "scroll", "<p>You need a body to do that.</p>")
    else
      Systems.Item.display_inventory(entity)
    end
  end
end
