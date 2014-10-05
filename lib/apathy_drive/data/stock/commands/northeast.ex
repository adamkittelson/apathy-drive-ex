defmodule Commands.Northeast do
  use Systems.Command

  def keywords, do: ["ne",  "northeast"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "northeast")
  end
end
