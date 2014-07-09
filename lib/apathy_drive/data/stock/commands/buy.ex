defmodule Commands.Buy do
  use Systems.Command

  def keywords, do: ["buy"]

  def execute(entity, arguments) do
    room = Parent.of(entity)

    Systems.Shop.buy(entity, room, Enum.join(arguments, " "))
  end
end
