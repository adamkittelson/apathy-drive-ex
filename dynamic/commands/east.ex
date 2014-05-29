defmodule Commands.East do
  use Systems.Command

  def keywords, do: ["e",  "east"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "east")
  end
end
