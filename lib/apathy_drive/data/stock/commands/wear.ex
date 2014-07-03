defmodule Commands.Wear do
  use Systems.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(entity, arguments) do
    if Components.Spirit.value(entity) == true do
      send_message(entity, "scroll", "<p>You need a body to do that.</p>")
    else
      Systems.Item.equip(entity, Enum.join(arguments, " "))
    end
  end
end
