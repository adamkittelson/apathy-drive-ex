defmodule Commands.Buy do
  use ApathyDrive.Command

  def keywords, do: ["buy"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    room = Monster.find_room(monster)

    Systems.Shop.buy(monster, room, Enum.join(arguments, " "))
  end
end
