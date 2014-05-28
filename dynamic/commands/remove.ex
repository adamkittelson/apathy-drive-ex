defmodule Commands.Remove do
  use Systems.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(entity, arguments) do
    Systems.Item.unequip(entity, Enum.join(arguments, " "))
  end
end
