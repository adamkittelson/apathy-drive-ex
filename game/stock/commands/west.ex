defmodule Commands.West do
  use Systems.Command

  def keywords, do: ["w",  "west"]

  def execute(entity, _arguments) do
    Systems.Room.move(entity, "west")
  end
end
