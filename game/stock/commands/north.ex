defmodule Commands.North do
  use Systems.Command

  def keywords, do: ["n", "north"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "north")
  end
end
