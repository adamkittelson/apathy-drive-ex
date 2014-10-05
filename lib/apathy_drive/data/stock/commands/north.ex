defmodule Commands.North do
  use Systems.Command

  def keywords, do: ["n", "north"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "north")
  end
end
