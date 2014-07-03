defmodule Commands.Down do
  use Systems.Command

  def keywords, do: ["d", "down"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "down")
  end
end
