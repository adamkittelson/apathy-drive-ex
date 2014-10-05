defmodule Commands.East do
  use Systems.Command

  def keywords, do: ["e",  "east"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "east")
  end
end
