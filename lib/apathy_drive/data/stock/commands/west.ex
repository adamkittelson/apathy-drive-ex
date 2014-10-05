defmodule Commands.West do
  use Systems.Command

  def keywords, do: ["w",  "west"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "west")
  end
end
