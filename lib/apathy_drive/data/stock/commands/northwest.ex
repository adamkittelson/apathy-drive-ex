defmodule Commands.Northwest do
  use Systems.Command

  def keywords, do: ["nw", "northwest"]

  def execute(spirit, monster, _arguments) do
    Systems.Room.move(spirit, monster, "northwest")
  end
end
