defmodule Commands.South do
  use Systems.Command

  def keywords, do: ["s",  "south"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "south")
  end
end
