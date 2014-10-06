defmodule Commands.Up do
  use Systems.Command

  def keywords, do: ["u", "up"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "up")
  end
end
