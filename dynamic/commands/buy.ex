defmodule Commands.Buy do
  use Systems.Command

  def keywords, do: ["buy"]

  def execute(entity, arguments) do
    current_room = Components.CurrentRoom.get_current_room(entity)

    Systems.Shop.buy(entity, current_room, Enum.join(arguments, " "))
  end
end
