defmodule Commands.Buy do
  use Systems.Command

  def keywords, do: ["buy"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to purchase things from a shop.</p>")
  end

  def execute(spirit, monster, arguments) do
    room = Parent.of(spirit)

    Systems.Shop.buy(monster, room, Enum.join(arguments, " "))
  end
end
