defmodule Commands.Remove do
  use Systems.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(entity, arguments) do
    if Components.Spirit.value(entity) == true do
      send_message(entity, "scroll", "<p>You need a body to do that.</p>")
    else
      Systems.Item.unequip(entity, Enum.join(arguments, " "))
    end
  end
end
