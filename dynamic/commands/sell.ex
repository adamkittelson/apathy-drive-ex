defmodule Commands.Sell do
  use Systems.Command

  def keywords, do: ["sell"]

  def execute(entity, arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)
    Systems.Shop.sell(entity, current_room, Enum.join(arguments, " "))
  end
end
