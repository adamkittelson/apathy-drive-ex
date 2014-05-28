defmodule Commands.Wear do
  use Systems.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(entity, arguments) do
    Systems.Item.equip(entity, Enum.join(arguments, " "))
  end
end
