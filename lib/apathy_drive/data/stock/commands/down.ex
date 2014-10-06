defmodule Commands.Down do
  use Systems.Command

  def keywords, do: ["d", "down"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "down")
  end
end
