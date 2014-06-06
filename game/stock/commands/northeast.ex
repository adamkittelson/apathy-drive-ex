defmodule Commands.NorthEast do
  use Systems.Command

  def keywords, do: ["ne",  "northeast"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "northeast")
  end
end
