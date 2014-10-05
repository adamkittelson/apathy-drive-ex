defmodule Commands.Southwest do
  use Systems.Command

  def keywords, do: ["sw", "southwest"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "southwest")
  end
end
