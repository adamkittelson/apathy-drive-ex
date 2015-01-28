defmodule Commands.Sell do
  use ApathyDrive.Command

  def keywords, do: ["sell"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(spirit)
    Systems.Shop.sell(monster, current_room, Enum.join(arguments, " "))
  end
end
