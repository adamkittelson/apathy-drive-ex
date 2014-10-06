defmodule Commands.South do
  use Systems.Command

  def keywords, do: ["s",  "south"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "south")
  end
end
