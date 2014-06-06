defmodule Commands.SouthWest do
  use Systems.Command

  def keywords, do: ["sw", "southwest"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "southwest")
  end
end
